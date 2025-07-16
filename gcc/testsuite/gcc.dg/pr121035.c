/* { dg-do compile } */
/* { dg-options "-O2 -fgimple" } */

int printf(const char *, ...);
int a, b, d;
unsigned c;
int __GIMPLE (ssa,startwith("pre"))
main ()
{
  unsigned int g;
  int f;
  unsigned int _1;
  unsigned int _2;
  int _3;
  int _4;
  int _5;
  unsigned int _6;
  unsigned int _7;
  int _10;
  unsigned int _11;
  _Bool _19;
  _Bool _20;
  _Bool _22;
  int _25;

  __BB(2):
  _25 = a;
  if (_25 != 0)
    goto __BB11;
  else
    goto __BB10;

  __BB(11):
  goto __BB3;

  __BB(3):
  f_26 = __PHI (__BB12: f_18, __BB11: 0);
  g_15 = c;
  if (f_26 != 0)
    goto __BB4;
  else
    goto __BB5;

  __BB(4):
  __builtin_putchar (48);
  goto __BB5;

  __BB(5):
  _1 = c;
  _2 = _1 << 1;
  _3 = a;
  _4 = d;
  _5 = _3 * _4;
  if (_5 != 0)
    goto __BB7;
  else
    goto __BB6;

  __BB(6):
  goto __BB7;

  __BB(7):
  _11 = __PHI (__BB5: 0u, __BB6: 4294967295u);
  _6 = g_15 * 4294967294u;
  _7 = _6 | _11;
  _20 = _3 != 0;
  _19 = _7 != 0u;
  _22 = _19 & _20;
  if (_22 != _Literal (_Bool) 0)
    goto __BB9;
  else
    goto __BB8;

  __BB(8):
  goto __BB9;

  __BB(9):
  _10 = __PHI (__BB7: 1, __BB8: 0);
  b = _10;
  f_18 = (int) _1;
  if (_3 != 0)
    goto __BB12;
  else
    goto __BB10;

  __BB(12):
  goto __BB3;

  __BB(10):
  return 0;

}


