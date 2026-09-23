/* { dg-do run } */
/* { dg-require-effective-target int32plus } */
/* { dg-options "-O2 -fgimple" } */

static int o;

__attribute__((noipa))
int
n ()
{
  return 129105662;
}


int __GIMPLE (ssa,startwith("vrp1"),guessed_local(1073741824))
main ()
{
  int am;
  int ak;
  int ay;
  int D_3044;
  int tem[1];
  int _10;
  int _11;
  int _12;
  int _21;
  int _23;
  int _24;
  int _25;
  int _26;
  int _27;
  int _29;

  __BB(2,guessed_local(1073741824)):
  tem[0ul] = _Literal (int) -1;
  o = 2147483645;
  goto __BB6(precise(134217728));

  __BB(10,guessed_local(17434151171)):
  goto __BB3(precise(134217728));

  __BB(3,loop_header(2),guessed_local(19522579055)):
  ak_20 = __PHI (__BB10: ak_22, __BB6: ak_30);
  am_7 = __PHI (__BB10: am_9, __BB6: am_31);
  ay_16 = __PHI (__BB10: ay_17, __BB6: ay_32);
be:
  am_9 = am_7 + 1;
  _10 = n ();
  _11 = _10 + _Literal (int) -129105632;
  _12 = am_9 << _11;
  if (_12 == 0)
    goto __BB4(guessed(126835753));
  else
    goto __BB5(guessed(7381975));

  __BB(4,guessed_local(18448837212)):
  ay_17 = ay_16 - _Literal (int) -2147483648;
  _21 = ak_20 + _Literal (int) -1;
  ak_22 = _21 + 2147483647;
  if (ak_22 != 0)
    goto __BB10(guessed(126835753));
  else
    goto __BB9(guessed(7381975));

  __BB(5,guessed_local(1073741842)):
  _23 = o;
  _24 = _Literal (int) -2 - _23;
  o = _24;
  _25 = ak_20 + _Literal (int) -1;
  __MEM <int> ((int *)&tem) = _25;
  _26 = ay_16 >> 31;
  _27 = ak_20 + _26;
  if (_27 != 2147483645)
    goto __BB7(guessed(67108864));
  else
    goto __BB8(guessed(67108864));

  __BB(9,guessed_local(1014686041)):
  goto __BB6(precise(134217728));

  __BB(6,loop_header(1),guessed_local(2088427865)):
  ak_30 = __PHI (__BB2: _Literal (int) -1, __BB9: 0);
  am_31 = __PHI (__BB2: _Literal (int) -1, __BB9: am_9);
  ay_32 = __PHI (__BB2: _Literal (int) -1782460880, __BB9: 3);
az:
  goto __BB3(precise(134217728));

  __BB(7,guessed_local(536870921)):
  goto __BB8(precise(134217728));

  __BB(8,guessed_local(1073741842)):
  _29 = __PHI (__BB5: 0, __BB7: 1);
  if (_29 != 0)
    goto __BB11;
  else
    goto __BB12;

  __BB(11):
    __builtin_abort ();

  __BB(12):
  return _29;

}



