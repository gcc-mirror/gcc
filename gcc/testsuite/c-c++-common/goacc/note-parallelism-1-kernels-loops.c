/* Test the output of "-fopt-info-optimized-omp" for an OpenACC 'kernels'
   construct containing loops.  */

/* { dg-additional-options "-fopt-info-optimized-omp" } */

//TODO update accordingly
/* See also "../../gfortran.dg/goacc/note-parallelism.f90".  */

int
main ()
{
  int x, y, z;

#pragma acc kernels /* { dg-message "note: assigned OpenACC seq loop parallelism" } */
 /* Strangely indented to keep this similar to other test cases.  */
 {
  for (x = 0; x < 10; x++) /* { dg-message "note: beginning .parloops. region in OpenACC .kernels. construct" } */
    ;

  for (x = 0; x < 10; x++)
    ;

  for (x = 0; x < 10; x++)
    for (y = 0; y < 10; y++)
      for (z = 0; z < 10; z++)
	;

  for (x = 0; x < 10; x++)
    ;

  for (x = 0; x < 10; x++)
    for (y = 0; y < 10; y++)
      ;

  for (x = 0; x < 10; x++)
    for (y = 0; y < 10; y++)
      for (z = 0; z < 10; z++)
	;

  for (x = 0; x < 10; x++)
    for (y = 0; y < 10; y++)
      for (z = 0; z < 10; z++)
	;
 }

  return 0;
}
