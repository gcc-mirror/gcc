/* { dg-do compile } */
/* { dg-options "-O -fgimple -ftree-slp-vectorize -mavx512f -fdump-tree-slp2" } */

void __GIMPLE (ssa,guessed_local(118111600))
bar (int * restrict a, int * restrict e,
     _Bool d0, _Bool d1, _Bool d2, _Bool d3, _Bool d4, _Bool d5, _Bool d6, _Bool d7)
{
  int _1;
  int _4;
  int _6;
  int _8;
  int _10;
  int _12;
  int _14;
  int _16;
  int _27;
  _Bool _37;
  _Bool _39;
  _Bool _41;
  int _43;
  _Bool _45;
  _Bool _47;
  _Bool _49;
  _Bool _53;
  _Bool _54;
  _Bool _55;
  int _56;
  _Bool _57;
  _Bool _58;
  _Bool _59;
  int _60;
  _Bool _61;
  _Bool _62;
  _Bool _63;
  int _64;
  _Bool _65;
  _Bool _66;
  _Bool _67;
  int _68;
  _Bool _69;
  _Bool _70;
  _Bool _71;
  int _72;
  _Bool _73;
  _Bool _74;
  _Bool _75;
  int _76;

  __BB(2,guessed_local(118111600)):
  _73 = d0_2(D);
  _69 = d1_5(D);
  _65 = d2_7(D);
  _61 = d3_9(D);
  _57 = d4_11(D);
  _53 = d5_13(D);
  _41 = d6_15(D);
  _49 = d7_17(D);
  a_81 = a_22(D);
  e_82 = e_23(D);
  _1 = __MEM <int> (a_81 + _Literal (int * restrict) 32);
  _4 = __MEM <int> (a_81 + _Literal (int * restrict) 36);
  _6 = __MEM <int> (a_81);
  _8 = __MEM <int> (a_81 + _Literal (int * restrict) 4);
  _10 = __MEM <int> (a_81 + _Literal (int * restrict) 48);
  _12 = __MEM <int> (a_81 + _Literal (int * restrict) 52);
  _14 = __MEM <int> (a_81 + _Literal (int * restrict) 16);
  _16 = __MEM <int> (a_81 + _Literal (int * restrict) 60);
  _74 = _1 != 0;
  _75 = _73 & _74;
  _76 = _75 ? _1 : 0;
  __MEM <int> (e_82) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 4) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 8) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 12) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 16) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 20) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 24) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 28) = _76;
  __MEM <int> (e_82 + _Literal (int * restrict) 32) = _76;
  _70 = _4 != 0;
  _71 = _69 & _70;
  _72 = _71 ? _4 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 36) = _72;
  _66 = _6 != 0;
  _67 = _65 & _66;
  _68 = _67 ? _6 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 40) = _68;
  _62 = _8 != 0;
  _63 = _61 & _62;
  _64 = _63 ? _8 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 44) = _64;
  _58 = _10 != 0;
  _59 = _57 & _58;
  _60 = _59 ? _10 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 48) = _60;
  _54 = _12 != 0;
  _55 = _53 & _54;
  _56 = _55 ? _12 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 52) = _56;
  _39 = _14 != 0;
  _37 = _39 & _41;
  _27 = _37 ? _14 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 56) = _27;
  _47 = _16 != 0;
  _45 = _47 & _49;
  _43 = _45 ? _16 : 0;
  __MEM <int> (e_82 + _Literal (int * restrict) 60) = _43;
  return;

}

/* We do not want a AVX512 mask CTOR built from converted _Bool.  */
/* { dg-final { scan-tree-dump-not " = \\(<signed-boolean:1>\\) " "slp2" } } */
