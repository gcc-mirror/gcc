// PR c++/19199

// { dg-do run }

// We used to turn the COND_EXPR lvalue into a MIN_EXPR rvalue, and
// then return a reference to a temporary in qMin.

#include <assert.h>

enum Foo { A, B };

template<typename T> T &qMin(T &a, T &b) 
{
  return a < b ? a : b;
}

int main (int,  char **)
{
  Foo f = A;
  Foo g = B;
  Foo &h = qMin(f, g);
  assert (&h == &f || &h == &g);
  const Foo &i = qMin((const Foo&)f, (const Foo&)g);
  assert (&i == &f || &i == &g);
  return 0;
}

