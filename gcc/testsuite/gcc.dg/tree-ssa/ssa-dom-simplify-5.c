/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fno-tree-vrp -fdump-tree-dom2" } */

/* x < y where x and y are tested for being unordered, so they are not;
   x == y where x <= y is tested, so it holds; and i < j where i != j
   is tested, so it holds.  */

void link_error (void);

void __GIMPLE (ssa,startwith("dom"))
by_lt (float x, float y)
{
  __BB(2):
  if (x_1(D) < y_2(D))
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  if (x_1(D) __UNORDERED y_2(D))
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

void __GIMPLE (ssa,startwith("dom"))
by_eq (float x, float y)
{
  __BB(2):
  if (x_1(D) == y_2(D))
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  if (x_1(D) <= y_2(D))
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

void __GIMPLE (ssa,startwith("dom"))
by_lt_int (int i, int j)
{
  __BB(2):
  if (i_1(D) < j_2(D))
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  if (i_1(D) != j_2(D))
    goto __BB5;
  else
    goto __BB4;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

/* { dg-final { scan-tree-dump-not "link_error" "dom2" } } */
