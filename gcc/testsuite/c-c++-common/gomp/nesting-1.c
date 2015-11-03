extern int i;

void
f_omp_parallel (void)
{
#pragma omp parallel
  {
#pragma omp parallel
    ;

#pragma omp target
    ;

#pragma omp target data map(i)
    ;

#pragma omp target update to(i)

#pragma omp target data map(i)
    {
#pragma omp parallel
      ;

#pragma omp target
      ;

#pragma omp target data map(i)
      ;

#pragma omp target update to(i)
    }
  }
}

void
f_omp_target (void)
{
#pragma omp target
  {
#pragma omp parallel
    ;
  }
}

void
f_omp_target_data (void)
{
#pragma omp target data map(i)
  {
#pragma omp parallel
    ;

#pragma omp target
    ;

#pragma omp target data map(i)
    ;

#pragma omp target update to(i)

#pragma omp target data map(i)
    {
#pragma omp parallel
      ;

#pragma omp target
      ;

#pragma omp target data map(i)
      ;

#pragma omp target update to(i)
    }
  }
}
