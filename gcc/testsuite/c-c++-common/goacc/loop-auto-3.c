/* Ensure that the auto clause falls back to seq parallelism when the
   OpenACC loop is not explicitly independent.  */

/* { dg-additional-options "-fopt-info-optimized-omp" } */

void
test ()
{
  int i, j, k, l, n = 100;
  
#pragma acc parallel loop auto /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  for (i = 0; i < n; i++)
#pragma acc loop auto independent /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
    for (j = 0; j < n; j++)
#pragma acc loop worker vector /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
      for (k = 0; k < n; k++)
	;

#pragma acc parallel loop auto independent /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
  for (i = 0; i < n; i++)
#pragma acc loop auto /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    for (j = 0; j < n; j++)
#pragma acc loop auto /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      for (k = 0; k < n; k++)
#pragma acc loop auto independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
	for (l = 0; l < n; l++)
	  ;

#pragma acc parallel loop gang /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
  for (i = 0; i < n; i++)
#pragma acc loop worker /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
    for (j = 0; j < n; j++)
#pragma acc loop vector /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
      for (k = 0; k < n; k++)
	{
#pragma acc loop auto independent /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	  /* { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } */
	  for (l = 0; l < n; l++)
	    ;
#pragma acc loop auto /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	  for (l = 0; l < n; l++)
	    ;
	}

#pragma acc parallel loop /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  /* { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } */
  for (i = 0; i < n; i++)
    {
#pragma acc loop gang worker /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
      for (j = 0; j < n; j++)
#pragma acc loop auto /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	for (k = 0; k < n; k++)
	  {
#pragma acc loop vector /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
	    for (l = 0; l < n; l++)
	      ;
#pragma acc loop auto independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
	    for (l = 0; l < n; l++)
	      ;
	  }
#pragma acc loop worker /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
      for (j = 0; j < n; j++)
#pragma acc loop vector /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
	for (k = 0; k < n; k++)
	  ;
    }

#pragma acc parallel loop /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
  for (i = 0; i < n; i++)
#pragma acc loop /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
    for (j = 0; j < n; j++)
#pragma acc loop /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      /* { dg-warning "insufficient partitioning available to parallelize loop" "" { target *-*-* } .-1 } */
      for (k = 0; k < n; k++)
#pragma acc loop /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
	  for (l = 0; l < n; l++)
	    ;
}
