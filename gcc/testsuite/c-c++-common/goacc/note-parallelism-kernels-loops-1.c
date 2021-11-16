/* Test the output of "-fopt-info-optimized-omp" for an OpenACC "kernels"
   construct containing loops.  */

/* { dg-additional-options "-fopt-info-optimized-omp" } */
/* { dg-additional-options "-fopt-info-note-omp" } */
/* { dg-additional-options "-O2" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

int
main ()
{
  int x, y, z;

#pragma acc kernels 
  for (x = 0; x < 10; x++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    ;

#pragma acc kernels
  for (x = 0; x < 10; x++)  /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    ;

#pragma acc kernels
  for (x = 0; x < 10; x++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    for (y = 0; y < 10; y++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	;

#pragma acc kernels
  for (x = 0; x < 10; x++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    ;

#pragma acc kernels
  for (x = 0; x < 10; x++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    for (y = 0; y < 10; y++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      ;

#pragma acc kernels
  for (x = 0; x < 10; x++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    for (y = 0; y < 10; y++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	;

#pragma acc kernels
  for (x = 0; x < 10; x++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */ \
    /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
    for (y = 0; y < 10; y++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++) /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
	;

  return 0;
}

/* { dg-prune-output ".auto. loop cannot be parallel" } */
