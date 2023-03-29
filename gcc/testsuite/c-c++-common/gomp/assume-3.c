/* { dg-do run } */
/* { dg-options "-fno-openmp -fopenmp-simd" } */

int i, j;

int
foo (void)
{
  j = 1;
  return 1;
}

int
main ()
{
  #pragma omp assume holds (i < 42)
  ;
  #pragma omp assume holds (++i == 1)
  ;
  if (i != 0)
    __builtin_abort ();
  #pragma omp assume holds (foo () == 1)
  ;
  if (j != 0)
    __builtin_abort ();
  return 0;
}
