// PR c++/50365
// { dg-options -std=c++0x }

struct A { int i; };

struct B {
  B();
  A* f();
};

B::B()
{
  int(f()->i);
}
