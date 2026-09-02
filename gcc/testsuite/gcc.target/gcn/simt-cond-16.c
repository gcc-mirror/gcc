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

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  A = __builtin_gcn_simt_enter (1, 2);

  res[TID] = 0;

  /* Complicated CFG without any loops.  */
  if (TID < 32)
    {
      res[TID] = 1;

      if (TID < 10)
	goto label;
    }
  else
    {
      res[TID] = 2;

label:
      res[TID] += 10;
    }

  __builtin_gcn_simt_exit (A);

  /* Use single-threading to check the results because broken multithread
     might also have broken checking.  */
  for (i = 0; i < 64; i++)
    if (i < 10 ? res[i] != 11
	: i < 32 ? res[i] != 1
	: res[i] != 12)
      __builtin_exit (1);
  return 0;
}
