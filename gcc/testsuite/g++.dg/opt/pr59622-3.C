// PR tree-optimization/59622
// { dg-do compile }
// { dg-options "-O2" }

struct C { int a; int b; };

namespace
{
  struct A
  {
    virtual C foo (); // { dg-warning "used but never defined" }
    C bar () { return foo (); }
  };
}

C
baz ()
{
  A a;
  return a.bar ();
}
