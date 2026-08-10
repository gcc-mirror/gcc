/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized" } */

int
round_up (int x)
{
  return ((x - 1) | 15) + 1;
}

/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
