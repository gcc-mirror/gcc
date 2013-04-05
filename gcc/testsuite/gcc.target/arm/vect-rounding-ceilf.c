/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_neon_ok } */
/* { dg-options "-O2 -ffast-math -ftree-vectorize" } */
/* { dg-add-options arm_v8_neon } */

#define N 32

void
foo (float *output, float *input)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = __builtin_ceilf (input[i]);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_call_ceilf } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
