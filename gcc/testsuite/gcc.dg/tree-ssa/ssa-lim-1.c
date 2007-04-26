/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim" } */

/* This is a variant that does cause fold to place a cast to
   int before testing bit 1.  */

void
quantum_toffoli (int control1, int control2, int target,
		 unsigned long *state, int size)
{
  int i;

  for(i=0; i<size; i++)
    {
       if (state[i] & ((unsigned long) 1 << control1))
         if (state[i] & ((unsigned long) 1 << control2))
           state[i] ^= ((unsigned long) 1 << target);
    }
}

/* { dg-final { scan-tree-dump-times "1 <<" 3 "lim" } } */
/* { dg-final { cleanup-tree-dump "lim" } } */
