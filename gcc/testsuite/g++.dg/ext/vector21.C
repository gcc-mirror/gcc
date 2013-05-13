/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple" } */

typedef int vec __attribute__ ((vector_size (4 * sizeof (int))));

void f1 (vec *x)
{
  *x = (*x >= 0) ? *x : -*x;
}
void f2 (vec *x)
{
  *x = (0 < *x) ? *x : -*x;
}
void g1 (vec *x)
{
  *x = (*x < 0) ? -*x : *x;
}
void g2 (vec *x)
{
  *x = (0 > *x) ? -*x : *x;
}
void h (vec *x, vec *y)
{
  *x = (*x < *y) ? *y : *x;
}
void i (vec *x, vec *y)
{
  *x = (*x < *y) ? *x : *y;
}
void j (vec *x, vec *y)
{
  *x = (*x < *y) ? *x : *x;
}

/* { dg-final { scan-tree-dump-times "ABS_EXPR" 4 "gimple" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR" "gimple" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR" "gimple" } } */
/* { dg-final { scan-tree-dump-not "VEC_COND_EXPR" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
