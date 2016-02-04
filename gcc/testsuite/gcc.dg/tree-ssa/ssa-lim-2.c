/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim2" } */

/* This is a variant that doesn't cause fold to place a cast to
   int before testing bit 1.  */

void
quantum_toffoli(int control1, int control2, int target, int *state,
int size)
{
  int i;

  for(i=0; i<size; i++)
    {
       if (state[i] & ( 1 << control1))
         if (state[i] & ( 1 << control2))
           state[i] ^= ( 1 << target);
    }
}

/* { dg-final { scan-tree-dump-times "1 <<" 3 "lim2" } } */
