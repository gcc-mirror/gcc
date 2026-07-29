/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* (X % C1) % C2 is X % C2 when C2 divides C1.  */

int f1 (int x) { return (x % 12) % 4; }
int f2 (int x) { return (x % 100) % 25; }
int f3 (int x) { return (x % -15) % 3; }
int f4 (int x) { return (x % 15) % -3; }
unsigned int f5 (unsigned int x) { return (x % 12) % 4; }

/* Each pair folds to one remainder.  */
/* { dg-final { scan-tree-dump-not " % 12;" "optimized" } } */
/* { dg-final { scan-tree-dump-times " % 4;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " % 25;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " % 3;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & 3;" 1 "optimized" } } */
