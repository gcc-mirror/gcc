/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fno-tree-vrp -fdump-tree-dom2" } */

void frob (void);

/* x <= 4 at the last test, so x > 3 holds for x == 4 only.  */

void __GIMPLE (ssa,startwith("dom"))
by_range (int x)
{
  __BB(2):
  if (x_1(D) > 5)
    goto __BB6;
  else
    goto __BB3;

  __BB(3):
  if (x_1(D) == 5)
    goto __BB6;
  else
    goto __BB4;

  __BB(4):
  if (x_1(D) > 3)
    goto __BB5;
  else
    goto __BB6;

  __BB(5):
  frob ();
  goto __BB6;

  __BB(6):
  return;
}

/* i <= j at the second test, so i >= j holds for i == j only.  */

void __GIMPLE (ssa,startwith("dom"))
by_relation (int i, int j)
{
  __BB(2):
  if (i_1(D) <= j_2(D))
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  if (i_1(D) >= j_2(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  frob ();
  goto __BB5;

  __BB(5):
  return;
}

/* a <= 10 and b >= 20 where the MIN is computed, so it is a.  */

int __GIMPLE (ssa,startwith("dom"))
by_range_min (int a, int b)
{
  int m;

  __BB(2):
  if (a_1(D) > 10)
    goto __BB5;
  else
    goto __BB3;

  __BB(3):
  if (b_2(D) < 20)
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  m_3 = __MIN (a_1(D), b_2(D));
  return m_3;

  __BB(5):
  return 0;
}

/* { dg-final { scan-tree-dump "if \\(x_1\\(D\\) == 4\\)" "dom2" } } */
/* { dg-final { scan-tree-dump "if \\(i_1\\(D\\) == j_2\\(D\\)\\)" "dom2" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "dom2" } } */
