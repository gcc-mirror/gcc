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

  if (1)
    res[TID] = 0;
  else
    {
label:
      res[TID] = 1000;
    }

  /* Loop with conditional goto preceeding label.  */
  for (i = 0; i < TID; i++)
    {
      if (TID > 30 && res[TID] < 1000)
	goto label;
      res[TID] += 1;
    }

  __builtin_gcn_simt_exit (A);

  /* Use single-threading to check the results because broken multithread
     might also have broken checking.  */
  for (i = 0; i < 64; i++)
    if (res[i] != (i > 30 ? i + 1000 : 93))
      __builtin_exit (1);
  return 0;
}
