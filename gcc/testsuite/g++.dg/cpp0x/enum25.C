// PR c++/56793
// { dg-require-effective-target c++11 }

struct A
{
  enum struct B {X, Y} b;
} a;

enum struct D {X,Y};
struct C { } c;

int main ()
{
  if (a.b == a.B::Y)
    a.b = A::B::X;

  c.D::Y;			// { dg-error "not a member" }
}
