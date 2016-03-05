extern int i;

void
f_omp (void)
{
#pragma omp parallel
  {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
  }

#pragma omp for
  for (i = 0; i < 3; i++)
    {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
      ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
      ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
      ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
      for (i = 0; i < 2; ++i)
	;
    }

#pragma omp sections
  {
    {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
      ;
    }
#pragma omp section
    {
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
      ;
    }
#pragma omp section
    {
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
      ;
    }
#pragma omp section
    {
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    }
#pragma omp section
    {
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    }
#pragma omp section
    {
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    }
#pragma omp section
    {
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
      for (i = 0; i < 2; ++i)
	;
    }
  }

#pragma omp single
  {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }

#pragma omp task
  {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }

#pragma omp master
  {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }

#pragma omp critical
  {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }

#pragma omp ordered
  {
#pragma acc parallel /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc data /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC construct inside of non-OpenACC region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }

#pragma omp target
  {
#pragma acc parallel /* { dg-error "OpenACC .parallel. construct inside of OpenMP .target. region" } */
    ;
#pragma acc kernels /* { dg-error "OpenACC .kernels. construct inside of OpenMP .target. region" } */
    ;
#pragma acc data /* { dg-error "OpenACC .data. construct inside of OpenMP .target. region" } */
    ;
#pragma acc update host(i) /* { dg-error "OpenACC .update. construct inside of OpenMP .target. region" } */
#pragma acc enter data copyin(i) /* { dg-error "OpenACC .enter/exit data. construct inside of OpenMP .target. region" } */
#pragma acc exit data delete(i) /* { dg-error "OpenACC .enter/exit data. construct inside of OpenMP .target. region" } */
#pragma acc loop /* { dg-error "loop directive must be associated with an OpenACC compute region" } */
    for (i = 0; i < 2; ++i)
      ;
  }
}

void
f_acc_parallel (void)
{
#pragma acc parallel
  {
#pragma omp parallel /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc parallel
  {
#pragma omp for /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    for (i = 0; i < 3; i++)
      ;
  }

#pragma acc parallel
  {
#pragma omp sections /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    {
      ;
    }
  }

#pragma acc parallel
  {
#pragma omp single /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc parallel
  {
#pragma omp task /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc parallel
  {
#pragma omp master /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc parallel
  {
#pragma omp critical /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc parallel
  {
#pragma omp ordered /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc parallel
  {
#pragma omp target /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
#pragma omp target data map(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
#pragma omp target update to(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
  }
}

void
f_acc_kernels (void)
{
#pragma acc kernels
  {
#pragma omp parallel /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc kernels
  {
#pragma omp for /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    for (i = 0; i < 3; i++)
      ;
  }

#pragma acc kernels
  {
#pragma omp sections /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    {
      ;
    }
  }

#pragma acc kernels
  {
#pragma omp single /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc kernels
  {
#pragma omp task /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc kernels
  {
#pragma omp master /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc kernels
  {
#pragma omp critical /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc kernels
  {
#pragma omp ordered /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc kernels
  {
#pragma omp target /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
#pragma omp target data map(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
#pragma omp target update to(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
  }
}

void
f_acc_data (void)
{
#pragma acc data
  {
#pragma omp parallel /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc data
  {
#pragma omp for /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    for (i = 0; i < 3; i++)
      ;
  }

#pragma acc data
  {
#pragma omp sections /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    {
      ;
    }
  }

#pragma acc data
  {
#pragma omp single /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc data
  {
#pragma omp task /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc data
  {
#pragma omp master /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc data
  {
#pragma omp critical /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc data
  {
#pragma omp ordered /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
  }

#pragma acc data
  {
#pragma omp target /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
#pragma omp target data map(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    ;
#pragma omp target update to(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
  }
}

void
f_acc_loop (void)
{
#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp parallel /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp for /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      for (i = 0; i < 3; i++)
	;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp sections /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      {
	;
      }
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp single /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp task /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp master /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp critical /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp ordered /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
    }

#pragma acc parallel
#pragma acc loop
  for (i = 0; i < 2; ++i)
    {
#pragma omp target /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
#pragma omp target data map(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
      ;
#pragma omp target update to(i) /* { dg-error "non-OpenACC construct inside of OpenACC region" } */
    }
}

#pragma acc routine
void
f_acc_routine (void)
{
#pragma omp target /* { dg-error "non-OpenACC construct inside of OpenACC routine" } */
  ;
}
