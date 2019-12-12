/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-forwprop -fno-tree-vrp" }
/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS / 2)
#else
#define N 64
#endif

int a[N], b[N], c[N];

void
foo ()
{
  int i;
  for (i = 0; i < N; i++)
    {
      long long d = a[i];
      long long e = b[i];
      d += e;
      c[i] = d;
    }
}

/* { dg-final { scan-tree-dump {vect_recog_over_widening_pattern: detected:[^\n]* \+} "vect" } } */
/* { dg-final { scan-tree-dump {VIEW_CONVERT_EXPR<vector[^ ]* unsigned} "vect" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
