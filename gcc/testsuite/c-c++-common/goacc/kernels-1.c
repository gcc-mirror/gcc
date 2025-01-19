// See also 'parallel-1.c', 'serial-1.c'.

/* { dg-additional-options "-fopt-info-optimized-omp" } */

int
kernels_empty (void)
{
#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  ;

  return 0;
}

int
kernels_eternal (void)
{
#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  {
    while (1)
      ;
  }

  return 0;
}

int
kernels_noreturn (void)
{
#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  __builtin_abort ();

  return 0;
}


float b[10][15][10];

void
kernels_loop_ptr_it (void)
{
  float *i;

#pragma acc kernels /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  {
#pragma acc loop
    for (i = &b[0][0][0]; i < &b[0][0][10]; i++)
      ;
  }
}
