/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */

int a[1], b;
short c;
unsigned short d;

int __GIMPLE (ssa,guessed_local(54066899),startwith("pre"))
main ()
{
  short int c_lsm_14;
  int b_lsm_13;
  short int _2;
  short int _10;
  short int _12;
  short int _18;
  int _22;
  short int _24;
  short int _27;
  short unsigned int _28;
  int _32;
  unsigned short _33;
  unsigned short _34;
  short int _35;
  short int _37;
  int _38;
  int _39;

  __BB(2,guessed_local(54066900)):
  _24 = c;
  if (_24 != _Literal (short int) 0)
    goto __BB3(guessed(36238787));
  else
    goto __BB8(guessed(97978941));

  __BB(3,guessed_local(14598063)):
  c = _Literal (short int) 0;
  _10 = c;
  if (_10 <= _Literal (short int) 1)
    goto __BB9(guessed(119453778));
  else
    goto __BB4(guessed(14763950));

  __BB(4,guessed_local(14598063)):
  _18 = __PHI (__BB13: _2, __BB3: _10);
  _28 = (short unsigned int) _18;
  d = _28;
  goto __BB8(precise(134217728));

  __BB(9,guessed_local(12992276)):
  b_lsm_13_26 = b;
  c_lsm_14_23 = c;
  goto __BB5(precise(134217728));

  __BB(5,loop_header(4),guessed_local(118111603)):
  _12 = __PHI (__BB11: _27, __BB9: _10);
  b_lsm_13_41 = 1;
  _22 = b_lsm_13_41;
  if (_22 != 0)
    goto __BB10(guessed(119453778));
  else
    goto __BB6(guessed(14763950));

  __BB(6,guessed_local(118111603)):
  b_lsm_13_31 = __PHI (__BB7: b_lsm_13_11, __BB5: b_lsm_13_41);
  _33 = (unsigned short) _12;
  _34 = _33 + _Literal (unsigned short) 1;
  _35 = (short int) _34;
  c_lsm_14_6 = _35;
  _27 = c_lsm_14_6;
  if (_27 <= _Literal (short int) 1)
    goto __BB11(guessed(119453778));
  else
    goto __BB13(guessed(14763950));

  __BB(13,guessed_local(12992276)):
  c_lsm_14_4 = __PHI (__BB6: c_lsm_14_6);
  _2 = __PHI (__BB6: _27);
  b_lsm_13_3 = __PHI (__BB6: b_lsm_13_31);
  b = b_lsm_13_3;
  c = c_lsm_14_4;
  goto __BB4(precise(134217728));

  __BB(11,guessed_local(105119327)):
  goto __BB5(precise(134217728));

  __BB(10,guessed_local(105119327)):
  _37 = _12 + _Literal (short int) 1;
  _38 = (int) _37;
  _39 = a[_38];
  goto __BB7(precise(134217728));

  __BB(7,loop_header(5),guessed_local(955630247)):
  b_lsm_13_11 = _39;
  _32 = b_lsm_13_11;
  if (_32 != 0)
    goto __BB12(guessed(119453778));
  else
    goto __BB6(guessed(14763950));

  __BB(12,guessed_local(850510920)):
  goto __BB7(precise(134217728));

  __BB(8,guessed_local(54066899)):
  return 0;

}


