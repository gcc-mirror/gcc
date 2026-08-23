/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

/* A shift by an in-range constant is perfectly well defined, so loop-invariant
   motion may hoist it out of a conditionally executed block just like any other
   arithmetic.  Only a constant count that is out of range (or a non-constant
   one, which cannot be proven in range here) has to be restricted to
   MOVE_PRESERVE_EXECUTION.

   Because the shift's result feeds the rest of the chain, restricting it also
   pinned everything computed from it, so the multiply and the divide below
   stayed in the loop as well.  */

void f (int *p, int n, int a, int b, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = ((a << 3) * b) / 7;
}

/* The whole invariant chain must move, exactly as it does when the shift is
   written as a multiply by 8.  */
/* { dg-final { scan-tree-dump-times "Moving statement" 3 "lim2" } } */
