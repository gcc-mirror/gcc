/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-mprefer-vector-width=128" { target { x86_64-*-* i?86-*-* } } } */

/* Ensure that SLP induction does not blindly reuse IVs when the scalar
   steps of the lanes differ.  */

#include "tree-vect.h"

#define N 16

static const int expected[N] = {
  1, 1, 1, 1, 1, 1, 1, 1,
  3, 3, 3, 3, 4, 4, 4, 4
};

__attribute__ ((noipa))
void
fill (int *ans, int rounds)
{
  int x = 1;
  int y = 1;
  int i = 0;

  while (rounds > 0)
    {
      ans[i++] = x;
      ans[i++] = x;
      ans[i++] = x;
      ans[i++] = x;
      ans[i++] = y;
      ans[i++] = y;
      ans[i++] = y;
      ans[i++] = y;

      --rounds;
      x += 2;
      y += 3;
    }
}

int
main (void)
{
  int ans[N] = { 0 };

  check_vect ();

  fill (ans, 2);

#pragma GCC novector
  for (int i = 0; i < N; ++i)
    if (ans[i] != expected[i])
      abort ();

  return 0;
}

/* Multi-lane SLP inductions are not yet supported for variable-length
   vectors.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_variable_length } } } */
