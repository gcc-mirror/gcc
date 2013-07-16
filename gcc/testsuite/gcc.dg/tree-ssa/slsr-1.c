/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

extern void foo (int);

void
f (int *p, unsigned int n)
{
  foo (*(p + n * 4));
  foo (*(p + 32 + n * 4));
  if (n > 3)
    foo (*(p + 16 + n * 4));
  else
    foo (*(p + 48 + n * 4));
}

/* { dg-final { scan-tree-dump-times "\\+ 128|\\, 128>" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 64|\\, 64>" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\+ 192|\\, 192>" 1 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
