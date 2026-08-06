/* { dg-do compile } */
/* { dg-options "-O2 -ftrapv -fdump-tree-optimized" } */

int
f (int x)
{
  return x + ((-x) & 15);
}

/* { dg-final { scan-tree-dump " -x" "optimized" } } */
