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
  res[TID] = 0;

  /* Simple if with no else.  */
  if (TID > 20)
    {
      res[TID] = TID;
    }
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
    if (res[i] != (i > 20 ? i : 0))
      __builtin_exit (1);
  return 0;
}
