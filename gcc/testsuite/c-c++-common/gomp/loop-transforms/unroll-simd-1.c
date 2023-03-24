/* { dg-options "-fno-openmp -fopenmp-simd" } */
/* { dg-do run } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-additional-options "-fdump-tree-omp_transform_loops" } */

#include <stdio.h>

int compute_sum1 ()
{
  int sum = 0;
  int i,j;

#pragma omp simd reduction(+:sum)
  for (i = 3; i < 10; ++i)
  #pragma omp unroll full
  for (j = -2; j < 7; ++j)
    sum++;

  if (j != 7)
    __builtin_abort;

  return sum;
}

int compute_sum2()
{
  int sum = 0;
  int i,j;
#pragma omp simd reduction(+:sum)
#pragma omp unroll partial(5)
  for (i = 3; i < 10; ++i)
  for (j = -2; j < 7; ++j)
    sum++;

  if (j != 7)
    __builtin_abort;

  return sum;
}

int compute_sum3()
{
  int sum = 0;
  int i,j;
#pragma omp simd reduction(+:sum)
#pragma omp unroll partial(1)
  for (i = 3; i < 10; ++i)
  for (j = -2; j < 7; ++j)
    sum++;

  if (j != 7)
    __builtin_abort;

  return sum;
}

int main ()
{
  int result = compute_sum1 ();
  if (result != 7 * 9)
    {
      fprintf (stderr, "%d: Wrong result %d\n", __LINE__, result);
      __builtin_abort ();
    }

  result = compute_sum1 ();
  if (result != 7 * 9)
    {
      fprintf (stderr, "%d: Wrong result %d\n", __LINE__, result);
      __builtin_abort ();
    }

  result = compute_sum3 ();
  if (result != 7 * 9)
    {
      fprintf (stderr, "%d: Wrong result %d\n", __LINE__, result);
      __builtin_abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump {omp loop_transform} "original" } } */
/* { dg-final { scan-tree-dump-not {omp loop_transform} "omp_transform_loops" } } */
