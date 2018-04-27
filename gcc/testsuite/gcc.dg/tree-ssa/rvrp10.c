/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fdisable-tree-thread1 -fdump-tree-rvrp -fno-tree-fre" } */

/* This is from PR14052.  */

int f2(int x) { return x == 1 || x == 3 || x == 1; }

/* { dg-final { scan-tree-dump "Branch rewritten" "rvrp" } } */
