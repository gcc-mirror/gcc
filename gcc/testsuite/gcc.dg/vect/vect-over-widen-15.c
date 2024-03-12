/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

#ifndef SIGNEDNESS
#define SIGNEDNESS signed
#define BASE_B -128
#define BASE_C -120
#endif

#define N 50

/* We rely on range analysis to show that these calculations can be done
   in SIGNEDNESS short, with the result being extended to int for the
   store.  */
void __attribute__ ((noipa))
f (int *restrict a, SIGNEDNESS char *restrict b,
   SIGNEDNESS char *restrict c)
{
  for (int i = 0; i < N; ++i)
    a[i] = (b[i] + c[i]) / 2;
}

int
main (void)
{
  check_vect ();

  int a[N];
  SIGNEDNESS char b[N], c[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = BASE_B + i * 5;
      c[i] = BASE_C + i * 4;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (a[i] != (BASE_B + BASE_C + i * 9) / 2)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {Splitting statement} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \+} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* / 2} "vect" } } */
/* { dg-final { scan-tree-dump-not {vect_recog_cast_forwprop_pattern: detected} "vect" } } */
/* { dg-final { scan-tree-dump {vector[^ ]* int} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
