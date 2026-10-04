/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fno-tree-vrp -fdump-tree-dom2" } */

/* b is 0 or 1, so the test against 5 is false.  */

void frob (void);

void __GIMPLE (ssa,startwith("dom"))
f (int x, int y)
{
  _Bool b;
  int i;

  __BB(2):
  b_1 = x_2(D) < y_3(D);
  i_4 = (int) b_1;
  if (i_4 == 5)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  frob ();
  goto __BB4;

  __BB(4):
  return;
}

/* { dg-final { scan-tree-dump "Folding predicate i_4 == 5 to 0" "dom2" } } */
