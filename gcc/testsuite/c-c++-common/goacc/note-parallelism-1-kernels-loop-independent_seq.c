/* Test the output of "-fopt-info-optimized-omp" for an OpenACC 'kernels'
   construct containing 'loop' constructs with 'independent' or 'seq'
   clauses.  */

/* { dg-additional-options "-fopt-info-all-omp" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

int
main ()
{
  int x, y, z;

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 {
#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent gang /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent worker /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent vector /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent gang vector /* { dg-message "optimized: assigned OpenACC gang vector loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent gang worker /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent worker vector /* { dg-message "optimized: assigned OpenACC worker vector loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent gang worker vector /* { dg-message "optimized: assigned OpenACC gang worker vector loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent gang /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent worker /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop independent vector /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang vector loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
    for (y = 0; y < 10; y++)
      ;

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC worker loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang worker loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC vector loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;

#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
  /* { dg-message "note: parallelized loop nest in OpenACC .kernels. region" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent /* { dg-message "optimized: assigned OpenACC gang vector loop parallelism" } */
    for (y = 0; y < 10; y++)
#pragma acc loop seq /* { dg-message "optimized: assigned OpenACC seq loop parallelism" } */
      for (z = 0; z < 10; z++)
	;
 }

  return 0;
}
