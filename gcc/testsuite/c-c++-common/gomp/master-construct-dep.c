// { dg-additional-options "-Wopenmp" }
int
main()
{
  int x = 0;
  #pragma omp parallel
  for (int i = 0; i < 8; ++i)
  {
    #pragma omp master // { dg-warning "'master' construct deprecated since OpenMP 5.1, use 'masked' \\\[-Wdeprecated-openmp\\\]" }
    {
      ++x;
    }
  }
  for (int i = 0; i < 8; ++i)
  {
    #pragma omp parallel master // { dg-warning "'master' construct deprecated since OpenMP 5.1, use 'masked' \\\[-Wdeprecated-openmp\\\]" }
    {
      ++x;
    }
  }
  return 0;
}
