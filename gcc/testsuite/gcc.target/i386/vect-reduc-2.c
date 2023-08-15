/* { dg-do compile } */
/* { dg-options "-fgimple -O2 -msse2 -fdump-tree-slp2-optimized" } */

signed char x[16];

signed char __GIMPLE (ssa,guessed_local(1073741824))
foo ()
{
  signed char _1;
  signed char _3;
  signed char _5;
  signed char _6;
  signed char _8;
  signed char _9;
  signed char _11;
  signed char _12;
  signed char _14;
  signed char _15;
  signed char _17;
  signed char _18;
  signed char _20;
  signed char _21;
  signed char _23;
  signed char _24;
  signed char _26;
  signed char _27;
  signed char _29;
  signed char _30;
  signed char _32;
  signed char _33;
  signed char _35;
  signed char _36;
  signed char _38;
  signed char _39;
  signed char _41;
  signed char _42;
  signed char _44;
  signed char _45;
  signed char _47;

  __BB(2,guessed_local(1073741824)):
  _1 = x[15];
  _3 = x[1];
  _5 = _1 + _3;
  _6 = x[2];
  _8 = _5 + _6;
  _9 = x[3];
  _11 = _8 + _9;
  _12 = x[4];
  _14 = _11 + _12;
  _15 = x[5];
  _17 = _14 + _15;
  _18 = x[6];
  _20 = _17 + _18;
  _21 = x[7];
  _23 = _20 + _21;
  _24 = x[8];
  _26 = _23 + _24;
  _27 = x[9];
  _29 = _26 + _27;
  _30 = x[10];
  _32 = _29 + _30;
  _33 = x[11];
  _35 = _32 + _33;
  _36 = x[12];
  _38 = _35 + _36;
  _39 = x[13];
  _41 = _38 + _39;
  _42 = x[14];
  _44 = _41 + _42;
  _45 = x[0];
  _47 = _44 + _45;
  return _47;

}

/* { dg-final { scan-tree-dump "optimized: basic block part vectorized" "slp2" } } */
