/* { dg-do compile } */
/* { dg-additional-options "-O3 -fdump-tree-vect-details -std=c99" } */

#define TYPE char
#define N 800

#pragma GCC target "+nosve"

TYPE a[N];

int foo ()
{
#pragma GCC unroll 32
  for (int i = 0; i < N; i++)
    if (a[i] == 124)
      return 1;

  return 0;
}

/* { dg-final { scan-tree-dump-not "VEC_TRUNC_ADD_HIGH" "vect" } } */
