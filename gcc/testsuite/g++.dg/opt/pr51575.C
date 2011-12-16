// PR tree-optimization/51575
// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions" }

#include <new>

struct S
{
  S ()
  {
    for (int i = 0; i < 3; ++i)
      new (&a[i]) double ();
  }
  double a[4];
};

void
foo ()
{
  S s;
}
