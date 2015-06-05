// PR tree-optimization/59622
// { dg-do compile }
// { dg-options "-O2" }

namespace
{
  struct A
  {
    virtual int foo (); // { dg-warning "used but never defined" }
    int bar () { return foo (); }
  };
}

int
baz ()
{
  A a;
  return a.bar ();
}
