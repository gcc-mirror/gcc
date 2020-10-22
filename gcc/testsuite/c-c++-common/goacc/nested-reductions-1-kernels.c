/* Test cases of nested 'reduction' clauses expected to compile cleanly.  */

/* See also 'gfortran.dg/goacc/nested-reductions-1-kernels.f90'. */

void acc_kernels (void)
{
  int i, j, k, sum, diff;

  /* FIXME:  These tests are not meaningful yet because reductions in
     kernels regions are not supported yet.  */
  #pragma acc kernels
  {
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(+:sum)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(+:sum)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;
  }
}
