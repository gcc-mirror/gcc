/* Check that __builtin_gcn_simt_broadcast_first_lane_di gives every active
   lane the 64-bit value of the first active lane, inside a SIMT region, in a
   conditional (where the first active lane is not lane 0), in a function
   called from a SIMT region, and for a pointer.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

/* Global variables aren't thread-local.  */
long in[64], all[64], odd[64], call[64];
long *ptr[64];

long __attribute__((noclone,noinline))
bcast (long x)
{
  return __builtin_gcn_simt_broadcast_first_lane_di (x);
}

void __attribute__((noclone,noinline))
f ()
{
  all[TID] = __builtin_gcn_simt_broadcast_first_lane_di (in[TID] * 3);
  if (TID & 1)
    odd[TID] = __builtin_gcn_simt_broadcast_first_lane_di (in[TID] + 1000);
  call[TID] = bcast (in[TID] + 100);
  ptr[TID] = (long *) __builtin_gcn_simt_broadcast_first_lane_di
    ((long) &in[TID]);
}

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  /* Make the high and low halves differ between lanes, and set the sign bit
     of the low half.  */
  for (i = 0; i < 64; i++)
    {
      in[i] = ((long) (i + 7) << 32) | (0x80000000u + i);
      odd[i] = -1;
    }

  A = __builtin_gcn_simt_enter (1, 2);
  f ();
  __builtin_gcn_simt_exit (A);

  for (i = 0; i < 64; i++)
    if (all[i] != in[0] * 3
	|| odd[i] != ((i & 1) ? in[1] + 1000 : -1)
	|| call[i] != in[0] + 100
	|| ptr[i] != &in[0])
      __builtin_abort ();

  return 0;
}
