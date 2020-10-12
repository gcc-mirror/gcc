/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1" } */

double y[2];
void foo (double x)
{
  y[0] = x * -3.;
  y[1] = x * 3.;
}
void bar (double x, double z)
{
  y[0] = -z / x;
  y[1] = z / x;
}

/* { dg-final { scan-tree-dump-times " \\* " 1 "fre1" } } */
/* { dg-final { scan-tree-dump-times " / " 1 "fre1" } } */
/* { dg-final { scan-tree-dump-times "= -_" 2 "fre1" } } */
