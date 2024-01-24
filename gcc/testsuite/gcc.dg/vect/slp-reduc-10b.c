/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fgimple" } */

int __GIMPLE (ssa,guessed_local(118111600),startwith("dce3"))
foo (int * x, int n)
{
  int i;
  int sum;
  int _1;
  __SIZETYPE__ _2;
  __SIZETYPE__ _3;
  int * _4;
  int _5;
  __SIZETYPE__ _7;
  __SIZETYPE__ _8;
  int * _9;
  int _100;
  __SIZETYPE__ _11;
  __SIZETYPE__ _12;
  int * _13;
  int _14;
  __SIZETYPE__ _15;
  __SIZETYPE__ _16;
  int * _17;
  int _18;

  __BB(2,guessed_local(118111600)):
  if (n_21(D) > 0)
    goto __BB5(guessed(119453778));
  else
    goto __BB7(guessed(14763950));

  __BB(5,guessed_local(105119324)):
  goto __BB3(precise(134217728));

  __BB(3,loop_header(1),guessed_local(955630224)):
  sum_30 = __PHI (__BB5: 0, __BB6: sum_27);
  i_32 = __PHI (__BB5: 0, __BB6: i_28);
  _1 = i_32 * 4;
  _2 = (__SIZETYPE__) _1;
  _3 = _2 * _Literal(__SIZETYPE__)4;
  _4 = x_23(D) + _3;
  _5 = __MEM <int> (_4);
  sum_24 = _5 + sum_30;
  _7 = _2 + _Literal(__SIZETYPE__)1;
  _8 = _7 * _Literal(__SIZETYPE__)4;
  _9 = x_23(D) + _8;
  _100 = __MEM <int> (_9);
  sum_25 = sum_24 + _100;
  _11 = _2 + _Literal(__SIZETYPE__)2;
  _12 = _11 * _Literal(__SIZETYPE__)4;
  _13 = x_23(D) + _12;
  _14 = __MEM <int> (_13);
  sum_26 = _14 + sum_25;
  _15 = _2 + _Literal(__SIZETYPE__)3;
  _16 = _15 * _Literal(__SIZETYPE__)4;
  _17 = x_23(D) + _16;
  _18 = __MEM <int> (_17);
  sum_27 = _18 + sum_26;
  i_28 = i_32 + 1;
  if (n_21(D) > i_28)
    goto __BB6(guessed(119453778));
  else
    goto __BB8(guessed(14763950));

  __BB(8,guessed_local(105119324)):
  goto __BB4(precise(134217728));

  __BB(6,guessed_local(850510900)):
  goto __BB3(precise(134217728));

  __BB(7,guessed_local(12992276)):
  goto __BB4(precise(134217728));

  __BB(4,guessed_local(118111601)):
  sum_31 = __PHI (__BB7: 0, __BB8: sum_27);
  return sum_31;

}

/* { dg-final { scan-tree-dump "Decided to SLP 1 instances" "vect" } } */
