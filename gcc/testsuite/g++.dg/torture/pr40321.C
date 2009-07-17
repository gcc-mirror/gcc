/* { dg-do compile } */

struct VectorD2
{
  VectorD2() : x(0), y(0) { }
  VectorD2(int _x, int _y) : x(_x), y(_y) { }
  int x, y;
  int GetLength2() const { return x*x + y*y; };
  VectorD2 operator+(const VectorD2 vec) const {
      return VectorD2(x+vec.x,y+vec.y);
  }
};
struct Shape
{
  enum Type { ST_RECT, ST_CIRCLE } type;
  VectorD2 pos;
  VectorD2 radius;
  bool CollisionWith(const Shape& s) const;
};
bool Shape::CollisionWith(const Shape& s) const
{
  if(type == ST_CIRCLE && s.type == ST_RECT)
    return s.CollisionWith(*this);
  return (pos + s.pos).GetLength2() < (radius + s.radius).GetLength2();
}
