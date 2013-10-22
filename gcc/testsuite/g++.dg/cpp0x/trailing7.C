// PR c++/50365
// { dg-options -std=c++11 }

struct A { int i; };

struct B {
  B();
  A* f();
};

B::B()
{
  int(f()->i);
}
