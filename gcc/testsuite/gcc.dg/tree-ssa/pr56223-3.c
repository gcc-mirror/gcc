/* PR tree-optimization/56223 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftrapv -fdump-tree-phiopt2" } */

int
add_abs (int s, int x)
{
  if (x >= 0)
    s += x;
  else
    s -= x;
  return s;
}

int
sub_abs (int s, int x)
{
  if (x >= 0)
    s -= x;
  else
    s += x;
  return s;
}

/* { dg-final { scan-tree-dump-not "ABSU_EXPR" "phiopt2" } } */
