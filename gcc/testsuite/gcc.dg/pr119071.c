/* PR rtl-optimization/119071 */
/* { dg-do run } */
/* { dg-options "-O2 -fgimple" } */

int a, b;

int __GIMPLE (ssa,startwith("expand"))
foo (void)
{
  int _1;
  int _2;
  int _3;
  int _5;
  _Bool _7;
  int _8;
  int _9;
  _Bool _14;
  int _15;
  int _16;
  _Bool _17;
  int _18;

  __BB(2):
  _1 = a;
  _17 = _1 != _Literal (int) -2;
  _18 = (int) _17;
  _2 = _18 + _Literal (int) -1;
  _3 = _2 + _18;
  _14 = _3 != 1;
  _15 = (int) _14;
  _16 = -_15;
  _7 = _3 == 1;
  _9 = (int) _7;
  _5 = _9 << _16;
  _8 = _5 - _15;
  b = _8;
  return _8;
}

int
main ()
{
  if (foo () != 1)
    __builtin_abort ();
}
