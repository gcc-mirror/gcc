// PR c++/61959
// { dg-do compile { target c++11 } }

template <class Coord> struct BasePoint
{
  Coord x, y;
  constexpr BasePoint (Coord, Coord) : x (0), y (0) {}
};
template <class T> struct BaseCoord
{
  int value;
  constexpr BaseCoord (T) : value (1) {}
};
template <class units> struct IntCoordTyped : BaseCoord<int>, units
{
  typedef BaseCoord Super;
  constexpr IntCoordTyped (int) : Super (0) {}
};
template <class units>
struct IntPointTyped : BasePoint<IntCoordTyped<units> >, units
{
  typedef BasePoint<IntCoordTyped<units> > Super;
  constexpr IntPointTyped (int, int) : Super (0, 0) {}
};
struct A
{
};
IntPointTyped<A> a (0, 0);
