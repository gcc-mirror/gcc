/* Test erroneous cases of nested 'reduction' clauses.  */

/* See also 'gfortran.dg/goacc/nested-reductions-2-routine.f90'. */

#pragma acc routine gang
void acc_routine (void)
{
  int i, j, k, l, sum, diff;

  {
    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop // { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop collapse(2) // { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      for (j = 0; j < 10; j++)
        for (k = 0; k < 10; k++)
          #pragma acc loop reduction(+:sum)
          for (l = 0; l < 10; l++)
            sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop // { dg-warning "nested loop in reduction needs reduction clause for .sum." }
      for (j = 0; j < 10; j++)
        #pragma acc loop // { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        // { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } 
	for (k = 0; k < 10; k++)
          #pragma acc loop reduction(+:sum)
          for (l = 0; l < 10; l++)
            sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(-:sum) // { dg-warning "conflicting reduction operations for .sum." }
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(+:sum) // { dg-warning "conflicting reduction operations for .sum." }
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(-:sum) // { dg-warning "conflicting reduction operations for .sum." }
      for (j = 0; j < 10; j++)
        #pragma acc loop reduction(-:sum)
        for (k = 0; k < 10; k++)
          sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(-:sum) // { dg-warning "conflicting reduction operations for .sum." }
      for (j = 0; j < 10; j++)
        #pragma acc loop // { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        // { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } 
        for (k = 0; k < 10; k++)
	  #pragma acc loop reduction(*:sum) // { dg-warning "conflicting reduction operations for .sum." }
	  for (l = 0; l < 10; l++)
	    sum = 1;

    #pragma acc loop reduction(+:sum)
    for (i = 0; i < 10; i++)
      #pragma acc loop reduction(-:sum) // { dg-warning "conflicting reduction operations for .sum." }
      for (j = 0; j < 10; j++)
      #pragma acc loop reduction(+:sum) // { dg-warning "conflicting reduction operations for .sum." })
      // { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } 
        for (k = 0; k < 10; k++)
	  #pragma acc loop reduction(*:sum) // { dg-warning "conflicting reduction operations for .sum." }
	  for (l = 0; l < 10; l++)
	    sum = 1;

    #pragma acc loop reduction(+:sum) reduction(-:diff)
    for (i = 0; i < 10; i++)
      {
        #pragma acc loop reduction(-:diff) // { dg-warning "nested loop in reduction needs reduction clause for .sum." }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(+:sum)
          for (k = 0; k < 10; k++)
            sum = 1;

        #pragma acc loop reduction(+:sum) // { dg-warning "nested loop in reduction needs reduction clause for .diff." }
        for (j = 0; j < 10; j++)
          #pragma acc loop reduction(-:diff)
          for (k = 0; k < 10; k++)
            diff = 1;
      }
  }
}
