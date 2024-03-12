/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-fre1 -fdump-tree-optimized" } */
/* part of PR tree-optimization/108397 */

long long
foo (unsigned char o)
{
  unsigned long long t1 = -(long long) (o == 0);
  unsigned long long t2 = -(long long) (t1 != 0);
  unsigned long long t3 = -(long long) (t1 <= t2);
  return t3;
}

/* Should be able to optimize this down to just `return -1;` during fre1. */
/* { dg-final { scan-tree-dump "return -1;" "fre1" } } */
/* FRE does not remove all dead statements so a few negate expressions are left behind. */
/* { dg-final { scan-tree-dump-not " -\[^1\]" "fre1" { xfail *-*-* } } } */

/* { dg-final { scan-tree-dump "return -1;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " - " "optimized" } } */
