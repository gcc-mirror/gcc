/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

int x0;

void __GIMPLE (ssa,startwith("pre"))
br (int yp, int oo)
{
  int oo_lsm_17;
  int x0_lsm_16;
  _Bool x0_lsm_15;
  int x0_lsm_14;
  int * qi;
  int _1;
  int _2;
  int _3;
  int _4;
  int _5;
  int _6;
  int _8;
  int _9;
  int _10;
  int _11;
  _Bool _12;
  int _13;
  int _14;
  int _15;
  int _16;
  int _17;

  __BB(2):
  _1 = oo;
  if (_1 == 0)
    goto __BB13;
  else
    goto __BB11;

  __BB(3):
  x0_lsm_14_44 = __PHI (__BB10: x0_lsm_14_50, __BB14: x0_lsm_14_25, __BB9: x0_lsm_14_50);
  goto __BB4;

  __BB(4):
  x0_lsm_14_39 = __PHI (__BB3: x0_lsm_14_44, __BB13: x0_lsm_14_38);
  _2 = x0_lsm_14_39;
  if (_2 != 0)
    goto __BB5;
  else
    goto __BB6;

  __BB(5):
  _3 = yp;
  x0_lsm_14_7 = _3;
  x0_lsm_15_47 = _Literal (_Bool) 1;
  goto __BB8;

  __BB(6):
  _4 = oo;
  if (_4 != 0)
    goto __BB7;
  else
    goto __BB9;

  __BB(7):
  _5 = yp;
  x0_lsm_14_48 = _5;
  x0_lsm_15_49 = _Literal (_Bool) 1;
  goto __BB8;

  __BB(8):
  x0_lsm_14_25 = __PHI (__BB5: x0_lsm_14_7, __BB7: x0_lsm_14_48);
  _6 = x0_lsm_14_25;
  if (_6 == 0)
    goto __BB9;
  else
    goto __BB14;

  __BB(9):
  __MEM <int> (qi_36) = 0;
  x0_lsm_14_50 = 0;
  x0_lsm_15_51 = _Literal (_Bool) 1;
  _10 = yp;
  _11 = oo;
  if (_10 == _11)
    goto __BB10;
  else
    goto __BB3;

  __BB(10):
  _12 = _11 != 0;
  _13 = (int) _12;
  _14 = _10 + _13;
  yp = _14;
  if (_14 == 0)
    goto __BB17;
  else
    goto __BB3;

  __BB(11):
  x0 = 1;
  oo_lsm_17_24 = oo;
  x0_lsm_16_19 = x0;
  goto __BB12;

  __BB(12):
  oo_lsm_17_41 = __PHI (__BB11: oo_lsm_17_24, __BB15: oo_lsm_17_43);
  _15 = oo_lsm_17_41;
  _16 = _15 + 1;
  oo_lsm_17_43 = _16;
  x0_lsm_16_42 = 1;
  _17 = x0_lsm_16_42;
  if (_17 <= 1)
    goto __BB15;
  else
    goto __BB16;

  __BB(15):
  goto __BB12;

  __BB(16):
  x0_lsm_16_52 = __PHI (__BB12: x0_lsm_16_42);
  oo_lsm_17_53 = __PHI (__BB12: oo_lsm_17_43);
  x0 = x0_lsm_16_52;
  oo = oo_lsm_17_53;
  goto __BB13;

  __BB(13):
  qi_36 = __PHI (__BB2: &yp, __BB16: &oo);
  x0_lsm_14_38 = x0;
  x0_lsm_15_23 = _Literal (_Bool) 0;
  goto __BB4;

  __BB(14):
  _8 = oo;
  _9 = _8 + 1;
  oo = _9;
  goto __BB3;

  __BB(17):
  return;
}
