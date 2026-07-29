/* { dg-do compile } */
/* { dg-options "-O2 -ftrapv -fdump-tree-optimized" } */

int
f (int x)
{
  return ((x + 1) & 7) == 0;
}

/* { dg-final { scan-tree-dump "\\+ 1" "optimized" } } */
