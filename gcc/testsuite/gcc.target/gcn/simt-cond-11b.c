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
  int i;

  res[TID] = 0;

  /* Loop with conditional goto following label.  */
  for (i = 0; i < 64; i++)
    {
      if (i == TID)
	goto label;
      res[TID] += 1;
    }

  res[TID] += 99999;  /* This should never happen.  */

label:
  ;
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
    if (res[i] != i)
      __builtin_exit (1);
  return 0;
}
