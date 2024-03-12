// PR c++/110745
// { dg-do compile { target c++17 } }
// { dg-options "-Wmissing-field-initializers" }

struct B { int i; };
struct D : B {
    int x;
    int y;
};

int
main ()
{
  D d = {.x=1, .y=2}; // { dg-warning "missing initializer for member .D::B." }
  (void)d;
}
