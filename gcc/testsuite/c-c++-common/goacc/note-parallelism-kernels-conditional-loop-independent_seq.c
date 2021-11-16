/* Test the output of "-fopt-info-note-omp" for OpenACC 'kernels'
   constructs containing conditionally executed 'loop' constructs with
   'independent' or 'seq' clauses.  */

/* { dg-additional-options "-fopt-info-note-omp" } */
/* { dg-additional-options "-fopt-info-missed-omp" } */
/* { dg-additional-options "--param openacc-kernels=decompose-parloops" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

extern int c;

int
main ()
{
  int x, y, z;

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop seq
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent gang
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent worker
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent vector
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent gang vector
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent gang worker
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent worker vector
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent gang worker vector
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent gang
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent worker
    for (y = 0; y < 10; y++)
#pragma acc loop independent vector
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
    ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
      ;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop independent
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop seq
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop independent
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop seq
    for (y = 0; y < 10; y++)
#pragma acc loop independent
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop independent
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop seq
      for (z = 0; z < 10; z++)
	;
 }

#pragma acc kernels
 /* Strangely indented to keep this similar to other test cases.  */
 if (c) /* { dg-message "note: beginning .parloops. part in OpenACC .kernels. region" } */
 {
#pragma acc loop seq
  /* { dg-message "missed: unparallelized loop nest in OpenACC .kernels. region: it's executed conditionally" "" { target *-*-* } .-1 } */
  for (x = 0; x < 10; x++)
#pragma acc loop independent
    for (y = 0; y < 10; y++)
#pragma acc loop seq
      for (z = 0; z < 10; z++)
	;
 }

  return 0;
}
