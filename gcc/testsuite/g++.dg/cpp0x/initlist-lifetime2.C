// Test that we properly extend the lifetime of the initializer_list
// array even if the initializer_list is a subobject.
// { dg-do run { target c++11 } }

#include <initializer_list>

extern "C" void abort();
bool ok;

bool do_throw;

struct A {
  A(int) { if (do_throw) throw 42; }
  ~A() { if (!ok) abort(); }
};

typedef std::initializer_list<A> AL;
typedef std::initializer_list<AL> AL2;
typedef std::initializer_list<AL2> AL3;

struct B {
  AL al;
  const AL& alr;
};

struct A2
{
  const A& a1;
  const A& a2;
};

struct C {
  AL ar[2];
  B b;
  AL3 al3;
  A2 a2;
  A2 a2r[2];
  C():
    ar{{1,2},{3,4}},
    b{{5,6},{7,8}},
    al3{{{1},{2},{3}}},
    a2{1,2},
    a2r{{1,2},{3,4}}
  { ok = true; }
};

struct D {
  AL ar[2] = {{1,2},{3,4}};
  B b = {{5,6},{7,8}};
  AL3 al3 = {{{1},{2},{3}}};
  A2 a2 = {1,2};
  A2 a2r[2] = {{1,2},{3,4}};
  D() { ok = true; }
};

volatile bool always_false = false;

int main(int argc, const char** argv)
{
  do_throw = always_false;	// always false, but optimizer can't tell
  ok = false;
  C c;
  ok = false;
  D d;
}
