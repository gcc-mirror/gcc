/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

struct a {
  short b;
  unsigned c;
} volatile d;
struct a e;
struct a f;
int __GIMPLE (ssa,guessed_local(214748368),startwith("slp"))
g (int ab)
{
  short e__lsm$13;
  short int _1;
  unsigned int _2;
  bool _3;
  bool _4;
  bool _5;
  short int _6;
  short int _7;
  bool _11;
  unsigned char _13;
  short int _14;
  unsigned char _16;
  unsigned char _17;
  bool _30;
  bool _31;
  short int _32;
  short int _33;
  short int _40;
  bool _41;
  unsigned char _43;
  bool _44;
  bool _45;
  short int _46;
  short int _47;
  short int _54;
  bool _55;
  unsigned char _57;
  bool _58;
  bool _59;
  short int _60;
  short int _61;

  __BB(2,guessed_local(214748370)):
  _2 = f.c;
  _13 = (unsigned char) _2;
  e__lsm$13_9 = __MEM <short> ((short *)&e);
  _14 = d.b;
  _5 = _14 == _Literal (short int) 0;
  _17 = _13 * _Literal (unsigned char) 252;
  _30 = _17 == _Literal (unsigned char) 6;
  _31 = _30 <= _5;
  _32 = (short int) _31;
  _33 = e__lsm$13_9 | _32;
  _40 = d.b;
  _41 = _40 == _Literal (short int) 0;
  _43 = _13 * _Literal (unsigned char) 253;
  _44 = _43 == _Literal (unsigned char) 6;
  _45 = _44 <= _41;
  _46 = (short int) _45;
  _47 = _33 | _46;
  _54 = d.b;
  _55 = _54 == _Literal (short int) 0;
  _57 = _13 * _Literal (unsigned char) 254;
  _58 = _57 == _Literal (unsigned char) 6;
  _59 = _58 <= _55;
  _60 = (short int) _59;
  _61 = _47 | _60;
  _1 = d.b;
  _11 = _1 == _Literal (short int) 0;
  _16 = -_13;
  _3 = _16 == _Literal (unsigned char) 6;
  _4 = _3 <= _11;
  _6 = (short int) _4;
  _7 = _6 | _61;
  __MEM <short> ((short *)&e) = _7;
  return;
}


