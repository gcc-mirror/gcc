/* PR rtl-optimization/127661 */
/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-dom2-details" } */

void bar (void);

/* On the edge to bb 3, DOM finds no use of Y that defines an SSA name.
   DOM then propagates Y into the definition of Z in bb 5, so the
   equivalence Y == 6 on the edge to bb 6 must still give Z == 10.  */

int __GIMPLE (ssa,startwith("dom"))
foo (int y, int r)
{
  int w;
  int z;

  __BB(2):
  if (y_5(D) == 1)
    goto __BB3;
  else
    goto __BB4;

  __BB(3):
  bar ();
  goto __BB4;

  __BB(4):
  w_2 = r_6(D) * 3;
  if (w_2 == y_5(D))
    goto __BB5;
  else
    goto __BB7;

  __BB(5):
  z_4 = w_2 + 4;
  if (y_5(D) == 6)
    goto __BB6;
  else
    goto __BB7;

  __BB(6):
  return z_4;

  __BB(7):
  return 0;
}

/* { dg-final { scan-tree-dump "COPY z_4 = 10" "dom2" } } */
