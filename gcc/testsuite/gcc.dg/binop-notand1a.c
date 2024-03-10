/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (char a, unsigned short b)
{
  return (a & !a) | (b & !b);
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized"  } } */
