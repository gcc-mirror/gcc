/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

extern void foo (int);

void
f (int *p, int n)
{
  foo (*(p + n++ * 4));
  foo (*(p + 32 + n++ * 4));
  foo (*(p + 16 + n * 4));
}

/* { dg-final { scan-tree-dump-times "\\+ 144" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 96" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
