/* { dg-do compile } */
/* { dg-options "-fgimple -O -mavx512f -ftree-vectorize -fno-vect-cost-model" } */

typedef _Bool sbool1 __attribute__((signed_bool_precision(1)));
typedef int v16si __attribute__((vector_size(64)));
typedef v16si v16sim __attribute__((vector_mask));
typedef long v16di __attribute__((vector_size(128)));

void __GIMPLE (ssa,guessed_local(118111600),startwith("slp"))
bar (int * restrict a, int * restrict d, int * restrict e)
{
  int * vectp_14;
  v16si * vectp_e_13;
  v16si vect_iftmp_12;
  v16sim mask__75_11;
  v16sim mask__74_10;
  v16si vect__6_9;
  v16si vect__1_8;
  int * vectp_7;
  v16si * vectp_a_6;
  int _2;
  int _5;
  int _7;
  int _9;
  int _11;
  int _13;
  int _15;
  int _17;
  _Bool _41;
  _Bool _49;
  _Bool _53;
  _Bool _57;
  _Bool _61;
  _Bool _65;
  _Bool _69;
  _Bool _73;
  sbool1 _135;
  sbool1 _136;
  sbool1 _137;
  sbool1 _138;
  sbool1 _139;
  sbool1 _140;
  sbool1 _141;
  sbool1 _142;
  sbool1 _143;
  sbool1 _144;
  sbool1 _145;
  sbool1 _146;
  sbool1 _147;
  sbool1 _148;
  sbool1 _149;
  sbool1 _150;
  v16sim _151;

  __BB(2,guessed_local(105119324)):
  _2 = __MEM <int> (d_26(D) + _Literal (int * restrict) 32);
  _73 = _2 != 0;
  _5 = __MEM <int> (d_26(D) + _Literal (int * restrict) 36);
  _69 = _5 != 0;
  _7 = __MEM <int> (d_26(D));
  _65 = _7 != 0;
  _9 = __MEM <int> (d_26(D) + _Literal (int * restrict) 4);
  _61 = _9 != 0;
  _11 = __MEM <int> (d_26(D) + _Literal (int * restrict) 48);
  _57 = _11 != 0;
  _13 = __MEM <int> (d_26(D) + _Literal (int * restrict) 52);
  _53 = _13 != 0;
  _15 = __MEM <int> (d_26(D) + _Literal (int * restrict) 16);
  _41 = _15 != 0;
  _17 = __MEM <int> (d_26(D) + _Literal (int * restrict) 60);
  _49 = _17 != 0;
  _135 = _49 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _136 = _41 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _137 = _53 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _138 = _57 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _139 = _61 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _140 = _65 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _141 = _69 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _142 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _143 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _144 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _145 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _146 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _147 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _148 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _149 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _150 = _73 ? _Literal (sbool1) -1 : _Literal (sbool1) 0;
  _151 = _Literal (v16sim) {_150, _149, _148, _147, _146, _145, _144, _143, _142, _141, _140, _139, _138, _137, _136, _135};
  vect__1_8_154 = __MEM <v16si, 32> ((int * restrict)a_22(D));
  vect_iftmp_12_158 = _151 ? vect__6_9_154 : _Literal (v16si) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  __MEM <v16si, 32> ((int * restrict)e_23(D)) = vect_iftmp_12_158;
  return;
}

