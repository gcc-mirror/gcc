// PR c++/49691
// { dg-options -std=c++0x }

struct A { int x; };
A* f();
struct B {
  void g()
  {
    int(f()->x);
  }
};
