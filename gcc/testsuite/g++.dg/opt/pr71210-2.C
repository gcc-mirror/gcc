// PR c++/71210
// { dg-do compile }
// { dg-options "-O2" }

struct C { int a; int b; C (); ~C (); };

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

void
foo ()
{
  C c = a->baz (0);
}
