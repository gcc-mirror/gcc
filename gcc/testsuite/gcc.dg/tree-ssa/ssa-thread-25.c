/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ethread-details" } */

/* This is vrp33 adapted to the backwards threader.  */

int f2(int x) { return x == 1 || x == 3 || x == 1; }

/* { dg-final { scan-tree-dump "about to thread: path: 2 -> 3, 3 -> 4" "ethread" } } */
