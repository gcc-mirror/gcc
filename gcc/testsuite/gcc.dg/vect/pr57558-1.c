/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

typedef unsigned int u_int;
void foo (u_int* __restrict x, u_int* __restrict y, u_int n)
{
  u_int i;
  for (i=1; i<=n; i++, x++, y++)
    *x += *y;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
