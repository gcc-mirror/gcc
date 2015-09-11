/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_call_floor } */

#define N 32

void
foo (double *output, double *input)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = __builtin_floor (input[i]);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target vect_call_floor } } } */
