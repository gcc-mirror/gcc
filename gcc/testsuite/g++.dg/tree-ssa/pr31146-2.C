/* { dg-do compile } */
/* { dg-options "-O -fno-tree-vrp -fdump-tree-forwprop1" } */

#include <new>

template <class T>
struct Vec
{
  Vec()
  {
    for (int i=0; i<3; ++i)
      new (&a[i]) T(0);
  }
  T a[3];
};

double foo (void)
{
  Vec<double> v;
  return v.a[2];
}

/* -std=c++17 and above doesn't emit operator new () != NULL, so there is
   nothing to fold anymore.  */
/* { dg-final { scan-tree-dump "Replaced .* != 0B. with .1" "forwprop1" { target c++14_down } } } */
