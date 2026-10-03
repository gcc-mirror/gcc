/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-evrp" } */

/* x & y and x | y are x when x == y: range-op records that relation
   when the statement is folded, and the test against x folds.  */

void link_error (void);

void __GIMPLE (ssa,startwith("evrp"))
and_it (int x, int y)
{
  int t;

  __BB(2):
  if (x_1(D) == y_2(D))
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  t_3 = x_1(D) & y_2(D);
  if (t_3 != x_1(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

void __GIMPLE (ssa,startwith("evrp"))
or_it (int x, int y)
{
  int t;

  __BB(2):
  if (x_1(D) == y_2(D))
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  t_3 = x_1(D) | y_2(D);
  if (t_3 != x_1(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
