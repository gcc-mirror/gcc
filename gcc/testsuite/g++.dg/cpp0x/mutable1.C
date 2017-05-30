// PR c++/77375
// { dg-do run { target c++11 } }

struct Base { mutable int i; };
struct Derived : Base {};
const Derived foo{};

int
main ()
{
  foo.i = 42;
}
