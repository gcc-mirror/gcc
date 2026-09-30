/* { dg-do link } */
/* { dg-options "-O2 -fgimple" } */

/* This is:

     if (i < 2 && tbl[i] > 5)
       link_error ();

   The index is [0, 1] so the range for v_3 should be [0, 0][5, 5].  */

static const int tbl[4] = { 0, 5, 6, 7 };

void link_error (void);

[[gnu::noipa]] void __GIMPLE (ssa,startwith("evrp"))
foo (unsigned int i)
{
  int v;

  __BB(2):
  if (i_2(D) < 2u)
    goto __BB3;
  else
    goto __BB5;

  __BB(3):
  v_3 = tbl[i_2(D)];
  if (v_3 > 5)
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  link_error ();
  goto __BB5;

  __BB(5):
  return;
}

int
main (void)
{
  foo (0);
  return 0;
}
