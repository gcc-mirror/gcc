void
f_omp_parallel (void)
{
#pragma omp parallel
  {
    int i;

#pragma acc loop
    for (i = 0; i < 2; ++i)
      ;
  }
}
