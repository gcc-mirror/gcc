/* PR tree-optimization/56098 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

volatile int *p;

void
foo (int x)
{
  *p = 1;
  if (x)
    *p = 2;
}

/* { dg-final { scan-tree-dump-not "=\[^\n\r]*\\*p" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
