// PR tree-optimization/59622
// { dg-do compile }
// { dg-options "-O2" }

namespace
{
  struct A
  {
    A () {}
    virtual A *bar (int) = 0;
    A *baz (int x) { return bar (x); }
  };
}

A *a;

void
foo ()
{
  a->baz (0);
}
