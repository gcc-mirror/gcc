/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

typedef unsigned long ul;
void foo (ul* __restrict x, ul* __restrict y, ul n)
{
  ul i;
  for (i=1; i<=n; i++, x++, y++)
    *x += *y;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
