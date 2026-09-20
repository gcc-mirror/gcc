/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-lim2-details" } */

/* An out-of-range constant count is reduced into range where the shift
   lands, as a non-constant one is.  */

int __GIMPLE (ssa, startwith ("lim"))
f (int x, int n)
{
  int i;
  int r;
  int _1;
  int _2;

  __BB(2):
  goto __BB6;

  __BB(3):
  _1 = i_5 & 1;
  if (_1 != 0)
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  _2 = x_8(D) << 33;
  r_9 = _2 / 5;
  goto __BB5;

  __BB(5):
  r_3 = __PHI (__BB4: r_9, __BB3: r_4);
  i_10 = i_5 + 1;
  goto __BB6;

  __BB(6,loop_header(1)):
  r_4 = __PHI (__BB2: 1, __BB5: r_3);
  i_5 = __PHI (__BB2: 0, __BB5: i_10);
  if (i_5 < n_7(D))
    goto __BB3;
  else
    goto __BB7;

  __BB(7):
  return r_4;
}

/* { dg-final { scan-tree-dump-times "Moving statement" 2 "lim2" } } */
/* { dg-final { scan-tree-dump " << 1;" "lim2" } } */
