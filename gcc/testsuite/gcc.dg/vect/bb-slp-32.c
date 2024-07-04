/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fvect-cost-model=dynamic" } */

#include "tree-vect.h"

int __attribute__((noipa))
foo (int * __restrict__ x, int *p, int a, int b)
{
  p = __builtin_assume_aligned (p, __BIGGEST_ALIGNMENT__);
  x = __builtin_assume_aligned (x, __BIGGEST_ALIGNMENT__);
  int tem0, tem1, tem2, tem3;
  int sum = 0;
  tem0 = p[0] + 1 + a;
  sum += tem0;
  x[0] = tem0;
  tem1 = p[1] + 2 + b;
  sum += tem1;
  x[1] = tem1;
  tem2 = p[2] + 3 + b;
  sum += tem2;
  x[2] = tem2;
  tem3 = p[3] + 4 + a;
  sum += tem3;
  x[3] = tem3;
  return sum;
}

int x[4] __attribute__((aligned(__BIGGEST_ALIGNMENT__)));
int p[4] __attribute__((aligned(__BIGGEST_ALIGNMENT__))) = { 0, 1, 2, 3 };

int main()
{
  check_vect ();

  if (foo (x, p, 7, 13) != 56)
    abort ();
  if (x[0] != 8 || x[1] != 16 || x[2] != 18 || x[3] != 14)
    abort ();
  return 0;
}

/* This is a weak test but we want to re-use the arithmetic for both the
   store and the reduction.  */
/* { dg-final { scan-tree-dump "re-using SLP tree" "slp2" { target { x86_64-*-* i?86-*-* } } } } */
/* On i386 we vectorize both the store and the reduction.  */
/* { dg-final { scan-tree-dump-times "basic block part vectorized" 2 "slp2" { target { x86_64-*-* i?86-*-* } } } } */
