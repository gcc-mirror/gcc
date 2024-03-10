// PR c++/110344
// { dg-do compile { target c++26 } }

struct A { int i; };
struct B { A a; };

constexpr int f()
{
  B b { 42 };
  void *p = &b;
  A* ap = static_cast<A*>(p);
  return ap->i;
}

static_assert (f() == 42);
