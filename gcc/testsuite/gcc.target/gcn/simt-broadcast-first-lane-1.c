/* Check that __builtin_gcn_simt_broadcast_first_lane gives every active lane
   the value of the first active lane, inside a SIMT region, in a
   conditional (where the first active lane is not lane 0), and in a
   function called from a SIMT region.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

/* Global variables aren't thread-local.  */
int in[64], all[64], odd[64], call[64];

int __attribute__((noclone,noinline))
bcast (int x)
{
  return __builtin_gcn_simt_broadcast_first_lane (x);
}

void __attribute__((noclone,noinline))
f ()
{
  all[TID] = __builtin_gcn_simt_broadcast_first_lane (in[TID] * 3);
  if (TID & 1)
    odd[TID] = __builtin_gcn_simt_broadcast_first_lane (in[TID] + 1000);
  call[TID] = bcast (in[TID] + 100);
}

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  for (i = 0; i < 64; i++)
    {
      in[i] = i + 7;
      odd[i] = -1;
    }

  A = __builtin_gcn_simt_enter (1, 2);
  f ();
  __builtin_gcn_simt_exit (A);

  for (i = 0; i < 64; i++)
    if (all[i] != 21
	|| odd[i] != ((i & 1) ? 1008 : -1)
	|| call[i] != 107)
      __builtin_abort ();

  return 0;
}
