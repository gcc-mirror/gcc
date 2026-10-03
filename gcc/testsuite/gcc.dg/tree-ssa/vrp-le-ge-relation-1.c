/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-evrp" } */

/* i >= j where i <= j holds is i == j.  */

void frob (void);

void __GIMPLE (ssa,startwith("evrp"))
f (int i, int j)
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

/* { dg-final { scan-tree-dump "if \\(i_1\\(D\\) == j_2\\(D\\)\\)" "evrp" } } */
