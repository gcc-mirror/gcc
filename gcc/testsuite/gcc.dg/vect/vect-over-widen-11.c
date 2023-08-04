/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-require-effective-target vect_pack_trunc } */
/* { dg-require-effective-target vect_unpack } */

#include "tree-vect.h"

#ifndef SIGNEDNESS
#define SIGNEDNESS signed
#define BASE_B -128
#define BASE_C -100
#endif

#define N 50

/* Both range analysis and backward propagation from the truncation show
   that these calculations can be done in SIGNEDNESS short, with "res"
   being extended for the store to d[i].  */
void __attribute__ ((noipa))
f (SIGNEDNESS char *restrict a, SIGNEDNESS char *restrict b,
   SIGNEDNESS char *restrict c, int *restrict d)
{
  for (int i = 0; i < N; ++i)
    {
      /* Deliberate use of signed >>.  */
      int res = b[i] + c[i];
      a[i] = (res + (res >> 1)) >> 2;
      d[i] = res;
    }
}

int
main (void)
{
  check_vect ();

  SIGNEDNESS char a[N], b[N], c[N];
  int d[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = BASE_B + i * 5;
      c[i] = BASE_C + i * 4;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c, d);
#pragma GCC novector
  for (int i = 0; i < N; ++i)
    {
      int res = BASE_B + BASE_C + i * 9;
      if (a[i] != ((res + (res >> 1)) >> 2))
	__builtin_abort ();
      if (d[i] != res)
	__builtin_abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump {Splitting statement} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \+ } "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 1} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 2} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_cast_forwprop_pattern: detected:[^\n]* \(signed char\)} "vect" } } */
/* { dg-final { scan-tree-dump {vector[^ ]* int} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
