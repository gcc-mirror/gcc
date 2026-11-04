/* Check that __builtin_gcn_simt_first_lane_p is true in the first active
   lane only: inside a SIMT region, in a conditional (where the first active
   lane is not lane 0), in a function called from a SIMT region, and in
   single-threaded code.  Also check that it works as a guard.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

/* Global variables aren't thread-local.  */
int all[64], odd[64], call[64];
int guarded;

int __attribute__((noclone,noinline))
first_lane_p ()
{
  return __builtin_gcn_simt_first_lane_p ();
}

void __attribute__((noclone,noinline))
f ()
{
  all[TID] = __builtin_gcn_simt_first_lane_p ();
  if (TID & 1)
    odd[TID] = __builtin_gcn_simt_first_lane_p ();
  call[TID] = first_lane_p ();
  if (__builtin_gcn_simt_first_lane_p ())
    guarded++;
}

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  for (i = 0; i < 64; i++)
    odd[i] = -1;

  if (!__builtin_gcn_simt_first_lane_p ())
    __builtin_abort ();

  A = __builtin_gcn_simt_enter (1, 2);
  f ();
  __builtin_gcn_simt_exit (A);

  for (i = 0; i < 64; i++)
    if (all[i] != (i == 0)
	|| odd[i] != ((i & 1) ? i == 1 : -1)
	|| call[i] != (i == 0))
      __builtin_abort ();
  if (guarded != 1)
    __builtin_abort ();

  return 0;
}
