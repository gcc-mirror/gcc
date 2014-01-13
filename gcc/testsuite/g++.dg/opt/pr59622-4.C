// PR tree-optimization/59622
// { dg-do compile }
// { dg-options "-O2" }

struct C { int a; int b; };

namespace
{
  struct A
  {
    A () {}
    virtual C bar (int) = 0;
    C baz (int x) { return bar (x); }
  };
}

A *a;

C
foo ()
{
  return a->baz (0);
}
