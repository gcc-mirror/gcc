/* Test cases of nested 'reduction' clauses expected to compile cleanly.  */

/* See also 'gfortran.dg/goacc/nested-reductions-1-serial.f90'. */

/* { dg-additional-options -Wuninitialized } */

void acc_serial (void)
{
  int i, j, k, sum, diff;

  #pragma acc serial
  /* implicit 'copy (sum, diff)'
     { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
     { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 } */
  {
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop collapse(2) reduction(+:sum)
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
      #pragma acc loop collapse(2) reduction(+:sum)
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

    #pragma acc loop reduction(+:sum) reduction(-:diff)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(-:diff)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }
  }
}

/* The same tests as above, but using a combined serial loop construct.  */

void acc_serial_loop (void)
{
  int i, j, k, l, sum, diff;

  #pragma acc serial loop
  /* implicit 'copy (sum, diff)'
     { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
     { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 } */
  for (int h = 0; h < 10; ++h)
  {
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop collapse(2) reduction(+:sum)
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
      #pragma acc loop collapse(2) reduction(+:sum)
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
      #pragma acc loop reduction(+:sum) // { dg-warning "insufficient partitioning available to parallelize loop" }
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum) reduction(-:diff)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(-:diff) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }
  }
}

/* The same tests as above, but now the outermost reduction clause is on
   the serial region, not the outermost loop.  */

void acc_serial_reduction (void)
{
  int i, j, k, sum, diff;

  #pragma acc serial reduction(+:sum)
  /* implicit 'copy (sum, diff)'
     { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
     { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 } */
  {
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    for (i = 0; i < 10; i++)
      #pragma acc loop
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        #pragma acc loop
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(+:sum)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum) reduction(-:diff)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(-:diff)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(-:diff)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum)
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }
  }
}

/* The same tests as above, but using a combined serial loop construct, and
   the outermost reduction clause is on that one, not the outermost loop.  */
void acc_serial_loop_reduction (void)
{
  int i, j, k, sum, diff;

  #pragma acc serial loop reduction(+:sum)
  /* implicit 'copy (sum, diff)'
     { dg-warning {'sum' is used uninitialized} TODO { xfail *-*-* } .-2 }
     { dg-warning {'diff' is used uninitialized} TODO { xfail *-*-* } .-3 } */
  for (int h = 0; h < 10; ++h)
  {
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    for (i = 0; i < 10; i++)
      #pragma acc loop
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        #pragma acc loop
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(+:sum) // { dg-warning "insufficient partitioning available to parallelize loop" }
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum) reduction(-:diff)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(-:diff) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(-:diff) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(+:sum) // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop // { dg-warning "insufficient partitioning available to parallelize loop" }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }
  }
}
