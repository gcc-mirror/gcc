// PR c++/5921
// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

struct A
{
  struct B { B(); };
};

static A::B b;

inline template <int i> void f (); // { dg-error "" }
