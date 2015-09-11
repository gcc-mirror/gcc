/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int *p;
int *foo (int *q, int i, int j)
{
  p = q + i;
  return p + j;
}

/* We shouldn't associate (q + i) + j to q + (i + j) here as we
   need q + i as well.  */

/* { dg-final { scan-tree-dump-times "\\+" 2 "optimized" } } */
