// { dg-do compile { target c++11 } }

struct Shape {
  enum class Type
  { Circle, Square };
};


void Foo (Shape &shape)
{
  +shape.Type::NOPE; // { dg-error "is not a member of" }
}
