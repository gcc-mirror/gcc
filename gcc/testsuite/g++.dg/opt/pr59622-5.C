// PR tree-optimization/59622
// { dg-do compile }
// { dg-options "-O2" }

namespace
{
  struct A
  {
    A () {}
    virtual A *bar (int);
    A *baz (int x) { return bar (x); }
  };

  __attribute__((noreturn)) A *A::bar (int)
  {
    __builtin_exit (0);
  }
}

A *a;

void
foo ()
{
  a->baz (0);
}
