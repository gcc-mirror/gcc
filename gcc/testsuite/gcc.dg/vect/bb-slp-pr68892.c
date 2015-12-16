/* { dg-do compile } */
/* { dg-additional-options "-fvect-cost-model=dynamic" } */

double a[128][128];
double b[128];

void foo(void)
{
  b[0] = a[0][0];
  b[1] = a[1][0];
  b[2] = a[2][0];
  b[3] = a[3][0];
}

/* { dg-final { scan-tree-dump "not profitable" "slp2" } } */
/* { dg-final { scan-tree-dump-times "Basic block will be vectorized" 0 "slp2" } } */
