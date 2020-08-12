// PR c++/90254
// { dg-do compile { target c++17 } }

struct A {
  A();
  A(const A &);
};
struct B : A { };

A foo ();

int
main ()
{
  B b{foo()};
}
