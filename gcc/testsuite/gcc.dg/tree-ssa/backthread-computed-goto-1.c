/* PR tree-optimization/126103 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-threadfull1-details" } */

/* Both predecessors of the computed goto block know the destination
   label exactly, so the backward threader must thread both paths
   through it.  */

int
f (int a)
{
  void *p;
  if (a)
    p = &&L0;
  else
    p = &&L1;
  goto *p;
L0:
  return 1;
L1:
  return 0;
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 2 "threadfull1" } } */
