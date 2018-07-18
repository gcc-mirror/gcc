/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int i)
{
  if (i > 0)
    i = -i;
  return i;
}

/* { dg-final { scan-tree-dump-not "ABS" "optimized" } } */
