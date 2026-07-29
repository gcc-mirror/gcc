/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Five does not divide twelve, so both remainders have to stay.  */
int f (int x) { return (x % 12) % 5; }

/* { dg-final { scan-tree-dump-times " % 12;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " % 5;" 1 "optimized" } } */
