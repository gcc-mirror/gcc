/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1-raw" } */

/* A - MIN (B, C) -> A + MAX (-B, -C) and the dual.  MIN (-x, -y) on its own
   is already folded by min (-A, -B) -> -max (A, B); these are the mixed and
   the multiple-use forms it does not reach.  */

int
sub_min_cst (int a, int x)
{
  int nx = -x;
  int m = nx < 5 ? nx : 5;
  return a - m;
}

int
sub_max_cst (int a, int x)
{
  int nx = -x;
  int m = nx > 5 ? nx : 5;
  return a - m;
}

int
sub_min_two_uses (int a, int x, int y, int *p)
{
  int nx = -x;
  int ny = -y;
  int m = nx < ny ? nx : ny;
  *p = nx;
  return a - m;
}

/* { dg-final { scan-tree-dump-not "<minus_expr," "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "<max_expr," 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "<min_expr," 1 "forwprop1" } } */
