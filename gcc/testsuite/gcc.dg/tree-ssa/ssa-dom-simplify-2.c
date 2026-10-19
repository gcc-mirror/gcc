/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fno-tree-vrp -fdump-tree-dom2" } */

/* x <= 5 and y >= 10 where x < y is computed, so it is 1.  */

int __GIMPLE (ssa,startwith("dom"))
by_range_cmp (int x, int y)
{
  _Bool b;
  int r;

  __BB(2):
  if (x_1(D) > 5)
    goto __BB5;
  else
    goto __BB3;

  __BB(3):
  if (y_2(D) < 10)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  b_3 = x_1(D) < y_2(D);
  r_4 = (int) b_3;
  return r_4;

  __BB(5):
  return 0;
}

/* 1 <= x <= 3 and 4 <= y <= 8 where x / y is computed, so it is 0.  */

int __GIMPLE (ssa,startwith("dom"))
by_range_div (int x, int y)
{
  int q;

  __BB(2):
  if (x_1(D) > 3)
    goto __BB7;
  else
    goto __BB3;

  __BB(3):
  if (x_1(D) < 1)
    goto __BB7;
  else
    goto __BB4;

  __BB(4):
  if (y_2(D) > 8)
    goto __BB7;
  else
    goto __BB5;

  __BB(5):
  if (y_2(D) < 4)
    goto __BB7;
  else
    goto __BB6;

  __BB(6):
  q_3 = x_1(D) / y_2(D);
  return q_3;

  __BB(7):
  return 7;
}

/* x == y where x & y and x - y are computed, so they are x and 0.  */

int __GIMPLE (ssa,startwith("dom"))
by_relation (int x, int y)
{
  int t;
  int d;
  int r;

  __BB(2):
  if (x_1(D) == y_2(D))
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  t_3 = x_1(D) & y_2(D);
  d_4 = x_1(D) - y_2(D);
  r_5 = t_3 + d_4;
  return r_5;

  __BB(4):
  return 0;
}

/* { dg-final { scan-tree-dump-not "x_1\\(D\\) < y_2\\(D\\)" "dom2" } } */
/* { dg-final { scan-tree-dump-not "x_1\\(D\\) / y_2\\(D\\)" "dom2" } } */
/* { dg-final { scan-tree-dump-not "x_1\\(D\\) & y_2\\(D\\)" "dom2" } } */
/* { dg-final { scan-tree-dump-not "x_1\\(D\\) - y_2\\(D\\)" "dom2" } } */
