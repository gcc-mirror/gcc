void
f_omp_parallel (void)
{
#pragma omp parallel
  {
    int i;

#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }
}
