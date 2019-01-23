/* Test the output of "-fopt-info-optimized-omp" for OpenACC 'kernels'
   constructs containing conditionally executed 'loop' constructs with
   'independent' or 'seq' clauses.  */

/* { dg-additional-options "-fopt-info-optimized-omp" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

extern int c;

int
main ()
{
  int x, y, z;

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop seq
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent gang
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent worker
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent vector
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent gang vector
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent gang worker
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent worker vector
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent gang worker vector
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent gang
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent worker
    for (y = 0; y < 10; y++)
#pragma acc loop independent vector
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
      ;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop independent
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop seq
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop independent
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop seq
    for (y = 0; y < 10; y++)
#pragma acc loop independent
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop independent
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop seq
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
 {
#pragma acc loop seq
  /* { dg-message "note: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop seq
      for (z = 0; z < 10; z++)
	;
 }

  return 0;
}
