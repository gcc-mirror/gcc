/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int
foo (int a)
{
  return (!a & 1) != ((a == 0) & 1);
}

/* { dg-final { scan-tree-dump-times "return 0" 1 "optimized" } } */
