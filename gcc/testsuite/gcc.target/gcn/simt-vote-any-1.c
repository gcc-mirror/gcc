/* Check that __builtin_gcn_simt_vote_any is nonzero in every active lane
   if any active lane passes a nonzero value, and zero otherwise: inside a
   SIMT region, in a conditional (where inactive lanes must not vote), in a
   function called from a SIMT region, and in single-threaded code.  */

/* { dg-do run } */
/* { dg-require-effective-target amdgcn_simt } */
/* { dg-additional-options "-O1" } */

#ifndef __SIMT__
#error "This test requires -msimt"
#endif

#define TID __builtin_gcn_dim_pos (2)

/* Global variables aren't thread-local.  */
int in[64], one[64], none[64], odd_in[64], odd_out[64], call[64];

int __attribute__((noclone,noinline))
vote_any (int x)
{
  return __builtin_gcn_simt_vote_any (x);
}

void __attribute__((noclone,noinline))
f ()
{
  one[TID] = __builtin_gcn_simt_vote_any (in[TID] == 37);
  none[TID] = __builtin_gcn_simt_vote_any (in[TID] == 64);
  if (TID & 1)
    {
      /* Lane 2 is inactive, so its vote must not count.  */
      odd_in[TID] = __builtin_gcn_simt_vote_any (in[TID] == 3);
      odd_out[TID] = __builtin_gcn_simt_vote_any (in[TID] == 2);
    }
  call[TID] = vote_any (in[TID] == 63);
}

/* Note: main enters in single-threaded mode.  */
int
main ()
{
  int i, A;

  for (i = 0; i < 64; i++)
    {
      in[i] = i;
      odd_in[i] = odd_out[i] = -1;
    }

  if (!__builtin_gcn_simt_vote_any (in[5])
      || __builtin_gcn_simt_vote_any (in[0]))
    __builtin_abort ();

  A = __builtin_gcn_simt_enter (1, 2);
  f ();
  __builtin_gcn_simt_exit (A);

  for (i = 0; i < 64; i++)
    if (!one[i]
	|| none[i]
	|| ((i & 1) ? !odd_in[i] : odd_in[i] != -1)
	|| ((i & 1) ? odd_out[i] : odd_out[i] != -1)
	|| !call[i])
      __builtin_abort ();

  return 0;
}
