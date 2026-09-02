/* Ensure that SIMT conditionals work per-lane.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

/* Global variables aren't thread-local.  */
int res[64];

void __attribute__((noclone,noinline))
f ()
{
  int i, j;

  res[TID] = 0;

  /* Nested loops with conditional early exit.  */
  for (i = 0; i < 64; i++)
    {
      for (j = 0; j < 64; j++)
	{
	  if (i == TID && j == TID)
	    goto label;

	  res[TID] += 1;  /* Executes TID*64 + TID times.  */
	}

      res[TID] += 1;      /* Executes TID times.  */
    }

  res[TID] += 1000;       /* Never executes.  */

label:
  res[TID] += 10000;      /* Executes once.  */
}

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  A = __builtin_gcn_simt_enter (1, 2);
  f ();
  __builtin_gcn_simt_exit (A);

  /* Use single-threading to check the results because broken multithread
     might also have broken checking.  */
  for (i = 0; i < 64; i++)
    if (res[i] != i*64 + i*2 + 10000)
      __builtin_exit (1);
  return 0;
}
