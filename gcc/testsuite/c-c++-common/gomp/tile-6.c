/* { dg-do run } */
/* { dg-options "-O0 -fopenmp-simd" } */

int
test1 (void)
{
  int sum = 0;
  for (int k = 0; k < 10; k++)
    {
      #pragma omp tile sizes(5,7)
      for (int i = 0; i < 10; i++)
	for (int j = 0; j < 10; j = j + 2)
	  sum = sum + 1;
  }

  return sum;
}

int
main ()
{
  int result = test1 ();

  if (result != 500)
    __builtin_abort ();
  return 0;
}
