/* { dg-do run } */
/* { dg-options "-fno-openmp -fopenmp-simd -fdump-tree-original -fdump-tree-gimple" } */

int
compute_sum1 (void)
{
  int sum = 0;
  int i, j;

  #pragma omp simd reduction(+:sum)
  for (i = 3; i < 10; ++i)
    #pragma omp unroll full
    for (j = -2; j < 7; ++j)
      sum++;

  if (i != 10 || j != 7)
    __builtin_abort ();

  return sum;
}

int
compute_sum2 (void)
{
  int sum = 0;
  int i, j;

  #pragma omp simd reduction(+:sum)
  #pragma omp unroll partial(5)
  for (i = 3; i < 10; ++i)
    for (j = -2; j < 7; ++j)
      sum++;

  if (i != 10 || j != 7)
    __builtin_abort ();

  return sum;
}

int
compute_sum3 (void)
{
  int sum = 0;
  int i, j;

  #pragma omp simd reduction(+:sum)
  #pragma omp unroll partial(1)
  for (i = 3; i < 10; ++i)
    for (j = -2; j < 7; ++j)
      sum++;

  if (i != 10 || j != 7)
    __builtin_abort ();

  return sum;
}

int
main ()
{
  if (compute_sum1 () != 7 * 9
      || compute_sum2 () != 7 * 9
      || compute_sum3 () != 7 * 9)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump {omp unroll} "original" } } */
/* { dg-final { scan-tree-dump-not {omp unroll} "gimple" } } */
