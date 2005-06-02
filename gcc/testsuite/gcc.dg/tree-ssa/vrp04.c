/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp" } */

foo (int a, int b)
{
  if (a == b)
    /* This should be folded to if (1)  */
    if (a == b)
      return a + b;
}

/* { dg-final { scan-tree-dump-times "Folding predicate a_.*to 1" 1 "vrp" } } */
/* { dg-final { cleanup-tree-dump "vrp" } } */
