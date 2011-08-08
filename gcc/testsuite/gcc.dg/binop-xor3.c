/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a, int b)
{
  return ((a && !b) || (!a && b));
}

/* { dg-final { scan-tree-dump-times "\\\^" 1 "optimized" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
