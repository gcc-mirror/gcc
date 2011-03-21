// Origin PR c++/40239
// { dg-do compile }

struct B { B() { } private: B(B const&); };
struct A { int a; B b; };

int
main()
{
  A a = {0};
}
