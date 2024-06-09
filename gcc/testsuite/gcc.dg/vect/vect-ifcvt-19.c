/* { dg-do compile } */
/* { dg-additional-options "-fgimple -Ofast -fdump-tree-ifcvt-raw" } */

void __GIMPLE (ssa,guessed_local(10737414), startwith ("fix_loops"))
foo (int * f, int d, int e)
{
  int t;
  int a;
  int i;
  __SIZETYPE__ _1;
  __SIZETYPE__ _2;
  int * _3;
  int _4;

  __BB(2,guessed_local(10737414)):
  goto __BB3(guessed(134217728));

  __BB(3,loop_header(1),guessed_local(1063004408)):
  i_18 = __PHI (__BB8: i_15, __BB2: 0);
  _1 = (__SIZETYPE__) i_18;
  _2 = _1 * _Literal(__SIZETYPE__) 4;
  _3 = f_9(D) + _2;
  a_10 = __MEM <int> (_3);
  if (a_10 < 0)
    goto __BB9(guessed(55029268));
  else
    goto __BB4(guessed(79188460));

  __BB(9,guessed_local(435831803)):
  goto __BB6(precise(134217728));

  __BB(4,guessed_local(627172605)):
  if (a_10 < e_11(D))
    goto __BB5(guessed(67108864));
  else
    goto __BB10(guessed(67108864));

  __BB(10,guessed_local(313586303)):
  goto __BB6(precise(134217728));

  __BB(5,guessed_local(313586302)):
  _4 = a_10 * d_12(D);
  t_13 = 1 - _4;
  goto __BB6(precise(134217728));

  __BB(6,guessed_local(1063004410)):
  t_6 = __PHI (__BB9: 1, __BB5: t_13, __BB10: 0);
  __MEM <int> (_3) = t_6;
  i_15 = i_18 + 1;
  if (i_15 != 1024)
    goto __BB8(guessed(132875551));
  else
    goto __BB7(guessed(1342177));

  __BB(8,guessed_local(1052374368)):
  goto __BB3(precise(134217728));

  __BB(7,guessed_local(10737416)):
  return;

}

/* { dg-final { scan-tree-dump-times {<cond_expr,} 2 ifcvt { target vect_int } } } */
