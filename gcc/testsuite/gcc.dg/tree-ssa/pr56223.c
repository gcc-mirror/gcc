/* PR tree-optimization/56223 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt2" } */

int
add_ge (int s, int x)
{
  if (x >= 0)
    s += x;
  else
    s -= x;
  return s;
}

int
add_gt (int s, int x)
{
  if (x > 0)
    s += x;
  else
    s -= x;
  return s;
}

int
add_le (int s, int x)
{
  if (x <= 0)
    s -= x;
  else
    s += x;
  return s;
}

int
add_lt (int s, int x)
{
  if (x < 0)
    s -= x;
  else
    s += x;
  return s;
}

int
sub_ge (int s, int x)
{
  if (x >= 0)
    s -= x;
  else
    s += x;
  return s;
}

int
sub_gt (int s, int x)
{
  if (x > 0)
    s -= x;
  else
    s += x;
  return s;
}

int
sub_le (int s, int x)
{
  if (x <= 0)
    s += x;
  else
    s -= x;
  return s;
}

int
sub_lt (int s, int x)
{
  if (x < 0)
    s += x;
  else
    s -= x;
  return s;
}

/* { dg-final { scan-tree-dump-times "ABSU_EXPR" 8 "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt2" } } */
