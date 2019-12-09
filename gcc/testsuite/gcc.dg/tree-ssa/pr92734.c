/* PR tree-optimization/92734 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* { dg-final { scan-tree-dump-times "return t_\[0-9]*\\\(D\\\);" 4 "forwprop1" } } */

int
f1 (int t)
{
  return 1 - (int) (1U - t);
}

int
f2 (int t)
{
  int a = 7U - t;
  return 7 - a;
}

int
f3 (int t)
{
  int a = 32U - t;
  return 32 - a;
}

int
f4 (int t)
{
  int a = 32 - t;
  return (int) (32 - (unsigned) a);
}
