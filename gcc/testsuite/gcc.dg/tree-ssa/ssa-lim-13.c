/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-lim2-details" } */

int __GIMPLE (ssa,startwith("lim"))
foo (int x, int n)
{
  int i;
  int r;
  int _1;
  int _2;
  int _6;

  __BB(2):
  goto __BB7;

  __BB(3):
  if (i_5 == 17)
    goto __BB8;
  else
    goto __BB4;

  __BB(4):
  _1 = i_5 & 1;
  if (_1 != 0)
    goto __BB5;
  else
    goto __BB6;

  __BB(5):
  _2 = __ABS x_8(D);
  r_9 = _2 / 5;
  goto __BB6;

  __BB(6):
  r_3 = __PHI (__BB5: r_9, __BB4: r_4);
  i_10 = i_5 + 1;
  goto __BB7;

  __BB(7,loop_header(1)):
  r_4 = __PHI (__BB2: 1, __BB6: r_3);
  i_5 = __PHI (__BB2: 0, __BB6: i_10);
  if (i_5 < n_7(D))
    goto __BB3;
  else
    goto __BB8;

  __BB(8):
  _6 = __PHI (__BB3: 0, __BB7: r_4);
  return _6;
}

/* { dg-final { scan-tree-dump-times "Moving statement" 2 "lim2" } } */
/* { dg-final { scan-tree-dump "ABSU_EXPR" "lim2" } } */
