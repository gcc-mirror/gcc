/* Test the output of "-fopt-info-optimized-omp" for OpenACC 'kernels'
   constructs containing 'loop' constructs with explicit or implicit 'auto'
   clause.  */

/* { dg-additional-options "-fopt-info-note-optimized-omp" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

int
main ()
{
  int x, y, z;

#pragma acc kernels
#pragma acc loop /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto gang /* { dg-optimized "assigned OpenACC gang loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto worker /* { dg-optimized "assigned OpenACC worker loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto vector /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto gang vector /* { dg-optimized "assigned OpenACC gang vector loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto gang worker /* { dg-optimized "assigned OpenACC gang worker loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto worker vector /* { dg-optimized "assigned OpenACC worker vector loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto gang worker vector /* { dg-optimized "assigned OpenACC gang worker vector loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto gang /* { dg-optimized "assigned OpenACC gang loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-2 } */
  for (x = 0; x < 10; x++)
#pragma acc loop auto worker /* { dg-optimized "assigned OpenACC worker loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
    for (y = 0; y < 10; y++)
#pragma acc loop auto vector /* { dg-optimized "assigned OpenACC vector loop parallelism" } */
  /* { dg-bogus ".auto. conflicts with other OpenACC loop specifiers" "" { xfail *-*-* } .-1 } */
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc kernels
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
      ;

#pragma acc kernels
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels
#pragma acc loop /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc kernels
#pragma acc loop /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: forwarded loop nest in OpenACC .kernels. region to .Graphite. for analysis" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop auto /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop /* { dg-optimized "assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

  return 0;
}
