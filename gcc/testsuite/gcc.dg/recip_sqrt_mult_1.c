/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-recip" } */

double res, res2, tmp;
void
foo (double a, double b)
{
  tmp = 1.0 / __builtin_sqrt (a);
  res = tmp * tmp;
  res2 = a * tmp;
}

/* { dg-final { scan-tree-dump "Optimizing reciprocal sqrt multiplications" "recip" } } */
/* { dg-final { scan-tree-dump "Replacing squaring multiplication" "recip" } } */
/* { dg-final { scan-tree-dump "Replacing original division" "recip" } } */
