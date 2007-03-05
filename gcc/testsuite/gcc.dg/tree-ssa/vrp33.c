/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

/* This is from PR14052.  */

int f2(int x) { return x == 1 || x == 3 || x == 1; }

/* { dg-final { scan-tree-dump "Folding predicate.*== 1 to 0" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
