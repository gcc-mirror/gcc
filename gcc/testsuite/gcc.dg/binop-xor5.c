/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a, int b, int c)
{
  return ((a & ~b & c) | (~a & b & c));
}

/* We expect to see "<bb N>"; confirm that, so that we know to count
   it in the real test.  */
/* { dg-final { scan-tree-dump-times "<bb\[^>\]*>" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\\^" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\&" 1 "optimized" } } */
