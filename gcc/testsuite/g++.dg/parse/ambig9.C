// PR c++/49691

struct A { int x; };
A* f();
struct B {
  void g()
  {
    int(f()->x);
  }
};
