// PR tree-optimization/59355
// { dg-do compile }
// { dg-options "-O2 -fno-devirtualize" }

struct S
{
  virtual void bar ();
};

void
foo (S *s)
{
  s->bar ();
}
