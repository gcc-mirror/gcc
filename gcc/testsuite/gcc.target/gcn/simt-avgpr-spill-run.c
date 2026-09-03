/* Ensure that the values spilled by simt-avgpr-spill-int.c keep their
   per-lane values.  A spill that kept lane 0 only would return the wrong
   value in every lane but the first.

   No -march is given here, so that the test runs on the GPU that is
   present; where that has no AVGPRs the spills go to memory instead, which
   has to preserve all 64 lanes just the same.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#include "simt-avgpr-spill-int.c"

/* Note: main enters in single-threaded mode.  */
int
main (void)
{
  int i, j;

  for (i = 0; i < 64 + N; i++)
    in[i] = i * 3 + 1;

  f ();

  /* Use single-threading to check the results because broken multithread
     might also have broken checking.  */
  for (j = 0; j < N; j++)
    for (i = 0; i < 64; i++)
      if (res[j][i] != (TYPE) ((i + j) * 3 + 1))
	__builtin_exit (1);

  return 0;
}
