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
   that these calculations can be done in SIGNEDNESS short.  */
void __attribute__ ((noipa))
f (SIGNEDNESS char *restrict a, SIGNEDNESS char *restrict b,
   SIGNEDNESS char *restrict c)
{
  /* Deliberate use of signed >>.  */
  for (int i = 0; i < N; ++i)
    a[i] = (b[i] + c[i]) >> 1;
}

int
main (void)
{
  check_vect ();

  SIGNEDNESS char a[N], b[N], c[N];
  for (int i = 0; i < N; ++i)
    {
      b[i] = BASE_B + i * 5;
      c[i] = BASE_C + i * 4;
      asm volatile ("" ::: "memory");
    }
  f (a, b, c);
  for (int i = 0; i < N; ++i)
    if (a[i] != (BASE_B + BASE_C + i * 9) >> 1)
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {Splitting statement} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \+ } "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* >> 1} "vect" } } */
/* { dg-final { scan-tree-dump {vect_recog_cast_forwprop_pattern: detected:[^\n]* \(signed char\)} "vect" } } */
/* { dg-final { scan-tree-dump-not {vector[^ ]* int} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
