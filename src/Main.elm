module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Events exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import OpenSolid.Svg as Svg
import OpenSolid.Polygon2d as Polygon2d exposing (Polygon2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import Random
import Window
import Debug


---- MODEL ----


type alias Model =
    { shapes : List Shape
    , spacing : Int
    , windowHeight : Float
    , windowWidth : Float
    , seed : Random.Seed
    }


type alias Shape =
    { id : Int
    , polygon : Polygon2d
    }


tessellate : Shape -> Int -> Maybe Shape
tessellate originalShape sideToTesselate =
    let
        originalLineSegments =
            Polygon2d.edges originalShape.polygon

        selectedLineSegment =
            get sideToTesselate originalLineSegments
    in
        case selectedLineSegment of
            Nothing ->
                Nothing

            Just lineSegment ->
                rotateAndTranslate originalShape lineSegment


rotateAndTranslate : Shape -> LineSegment2d -> Maybe Shape
rotateAndTranslate originalShape originalLineSegment =
    let
        matchingLineSegments =
            List.filter (\edge -> LineSegment2d.squaredLength edge == LineSegment2d.squaredLength originalLineSegment) (Polygon2d.edges originalShape.polygon)

        selectedLineSegment =
            Maybe.withDefault (LineSegment2d.fromEndpoints ( Point2d.fromCoordinates ( 1, 2 ), Point2d.fromCoordinates ( 3, 4 ) )) (List.head matchingLineSegments)

        originalLineSegmentDirection =
            LineSegment2d.direction originalLineSegment

        selectedLineSegmentDirection =
            LineSegment2d.direction selectedLineSegment

        boundingBox =
            originalShape.polygon |> Polygon2d.boundingBox

        newShape =
            case ( originalLineSegmentDirection, selectedLineSegmentDirection, boundingBox ) of
                ( Just od, Just sd, Just bb ) ->
                    Just
                        { originalShape | polygon = Polygon2d.rotateAround (BoundingBox2d.centroid bb) ((Direction2d.angle od) + (degrees 180) - (Direction2d.angle sd)) originalShape.polygon }

                ( _, _, _ ) ->
                    Nothing

        rotatedLineSegment =
            case ( originalLineSegmentDirection, selectedLineSegmentDirection, boundingBox ) of
                ( Just od, Just sd, Just bb ) ->
                    Just (LineSegment2d.rotateAround (BoundingBox2d.centroid bb) ((Direction2d.angle od) + (degrees 180) - (Direction2d.angle sd)) selectedLineSegment)

                ( _, _, _ ) ->
                    Nothing
    in
        case ( newShape, rotatedLineSegment, boundingBox ) of
            ( Just shape, Just rls, Just bb ) ->
                translate originalShape shape originalLineSegment rls bb

            ( _, _, _ ) ->
                Nothing


translate : Shape -> Shape -> LineSegment2d -> LineSegment2d -> BoundingBox2d -> Maybe Shape
translate originalShape newShape originalLineSegment newLineSegment bb =
    let
        border =
            10

        centroid =
            BoundingBox2d.centroid bb

        originalMidpoint =
            LineSegment2d.midpoint originalLineSegment

        newMidpoint =
            LineSegment2d.midpoint newLineSegment

        vectorCentroidToOriginalMidpoint =
            Vector2d.from centroid originalMidpoint

        vectorNewMidpointToCentroid =
            Vector2d.from newMidpoint centroid

        directionOfCentroidToOriginalMidpoint =
            Vector2d.direction vectorCentroidToOriginalMidpoint

        vectorBorder =
            case directionOfCentroidToOriginalMidpoint of
                Just direction ->
                    Just (Vector2d.with { length = border, direction = direction })

                Nothing ->
                    Nothing

        interpolatedVector =
            case vectorBorder of
                Just vector ->
                    Just (Vector2d.sum (Vector2d.sum vectorCentroidToOriginalMidpoint vector) vectorNewMidpointToCentroid)

                Nothing ->
                    Nothing
    in
        case interpolatedVector of
            Just vec ->
                Just { newShape | polygon = Polygon2d.translateBy vec newShape.polygon }

            Nothing ->
                Nothing


get : Int -> List a -> Maybe a
get index list =
    -- 3 [ 1, 2, 3, 4, 5, 6 ]
    if (List.length list) >= index then
        List.take index list
            -- [ 1, 2, 3 ]
            |>
                List.reverse
            -- [ 3, 2, 1 ]
            |>
                List.head
        -- Just 3
    else
        Nothing


init : ( Model, Cmd Msg )
init =
    let
        diamond =
            Shape 1
                (Polygon2d.fromVertices
                    [ Point2d.fromCoordinates ( 100, 100 )
                    , Point2d.fromCoordinates ( 150, 130 )
                    , Point2d.fromCoordinates ( 200, 100 )
                    , Point2d.fromCoordinates ( 150, 70 )
                    ]
                )

        badTriangle =
            Shape 1
                (Polygon2d.fromVertices
                    [ Point2d.fromCoordinates ( 350, 350 )
                    , Point2d.fromCoordinates ( 370, 380 )
                    , Point2d.fromCoordinates ( 350, 380 )
                    ]
                )

        goodTriangle =
            Shape 1
                (Polygon2d.fromVertices
                    [ Point2d.fromCoordinates ( 250, 60 )
                    , Point2d.fromCoordinates ( 200, 400 )
                    , Point2d.fromCoordinates ( 300, 400 )
                    ]
                )

        weird =
            Shape 1
                (Polygon2d.fromVertices
                    [ Point2d.fromCoordinates ( 150, 150 )
                    , Point2d.fromCoordinates ( 170, 280 )
                    , Point2d.fromCoordinates ( 190, 300 )
                    , Point2d.fromCoordinates ( 300, 270 )
                    , Point2d.fromCoordinates ( 300, 160 )
                    ]
                )
    in
        ( { shapes = [ goodTriangle ]
          , windowHeight = 800.0
          , windowWidth = 1400.0
          , spacing = 5
          , seed = Random.initialSeed 54
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = Tessellate Shape
    | Resize Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tessellate shape ->
            let
                numberOfEdges =
                    List.length (Polygon2d.edges shape.polygon)

                ( sidePicked, seed ) =
                    Random.step (Random.int 1 numberOfEdges) model.seed

                newShape =
                    tessellate shape sidePicked
            in
                case newShape of
                    Just shape ->
                        ( { model | shapes = (shape :: model.shapes), seed = seed }, Cmd.none )

                    Nothing ->
                        ( { model | seed = seed }, Cmd.none )

        Resize width height ->
            ( { model | windowWidth = width / 1.0, windowHeight = height / 1.0 }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        svg =
            Svg.g
                []
            <|
                List.map
                    (\shape ->
                        Svg.polygon2d
                            [ Attributes.stroke "blue"
                            , Attributes.fill "orange"
                            , Attributes.strokeWidth "5"
                            , Attributes.strokeMiterlimit "500"
                            , onClick (Tessellate shape)
                            ]
                            shape.polygon
                    )
                    model.shapes
    in
        div []
            [ Svg.render2d (BoundingBox2d.with { minX = 0.0, maxX = model.windowWidth, minY = 0.0, maxY = model.windowHeight }) svg
            ]


subscription : Model -> Sub Msg
subscription model =
    Window.resizes (\size -> Resize size.width size.height)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
