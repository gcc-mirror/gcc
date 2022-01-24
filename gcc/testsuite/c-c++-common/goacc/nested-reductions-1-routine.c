/* Test cases of nested 'reduction' clauses expected to compile cleanly.  */

/* See also 'gfortran.dg/goacc/nested-reductions-1-routine.f90'. */

/* { dg-additional-options -Wuninitialized } */

#pragma acc routine gang
void acc_routine (void)
{
  int i, j, k, sum, diff;

  {
    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop reduction(+:sum)
    /* { dg-warning {'sum' is used uninitialized} {} { target *-*-* } .-1 } */
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop collapse(2) reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(+:sum)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop collapse(2) reduction(+:sum)
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          sum = 1;

    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(+:sum)
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    /* { dg-error "gang reduction on an orphan loop" "" { target *-*-* } .+1 } */
    #pragma acc loop reduction(+:sum) reduction(-:diff)
    /* { dg-warning {'diff' is used uninitialized} {} { target *-*-* } .-1 } */
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
