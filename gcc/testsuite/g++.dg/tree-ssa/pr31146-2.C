/* { dg-do compile } */
/* { dg-options "-O -fcheck-new -fno-tree-vrp -fdump-tree-forwprop1 -fno-thread-jumps" } */

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

/* GCC 8 emits operator new () != NULL with -fcheck-new. */
/* { dg-final { scan-tree-dump-times " != 0B\\)" 1 "forwprop1" } } */
