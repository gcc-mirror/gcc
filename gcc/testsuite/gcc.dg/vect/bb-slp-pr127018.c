/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */
/* { dg-additional-options "-msse4" { target sse4 } } */

char* mm;
char m;

void __GIMPLE (ssa,guessed_local(1073741822),startwith("slp"))
decode_endpoint (int * pEndpoints, int index)
{
  int tt;
  int t;
  int y1;
  int v3;
  int v1;
  int v0;
  char _1;
  char * _2;
  char _3;
  char _4;

  __BB(2,guessed_local(1073741822)):
  if (index_8(D) == 0)
    goto __BB9(guessed(45634028));
  else
    goto __BB3(guessed(88583700));

  __BB(9,guessed_local(365072223)):
  goto __BB8(precise(134217728));

  __BB(3,guessed_local(708669599)):
  _1 = m;
  v0_10 = (int) _1;
  _2 = mm;
  _3 = __MEM <char> (_2 + _Literal (char *) 1);
  v1_11 = (int) _3;
  if (index_8(D) == 3)
    goto __BB4(guessed(45634028));
  else
    goto __BB5(guessed(88583700));

  __BB(4,guessed_local(240947666)):
  _4 = __MEM <char> (_2 + _Literal (char *) 3);
  v3_20 = (int) _4;
  __MEM <int[2]> (pEndpoints_15(D) + _Literal (int *) 16)[0] = v0_10;
  __MEM <int[2]> (pEndpoints_15(D) + _Literal (int *) 16)[1] = v1_11;
  __MEM <int[2]> (pEndpoints_15(D) + _Literal (int *) 24)[1] = v3_20;
  __MEM <int[2]> (pEndpoints_15(D) + _Literal (int *) 24)[0] = v3_20;
  goto __BB8(precise(134217728));

  __BB(5,guessed_local(467721933)):
  t_12 = v0_10 << 4;
  tt_13 = v1_11 << 4;
  if (v0_10 <= v1_11)
    goto __BB10(guessed(67108864));
  else
    goto __BB6(guessed(67108864));

  __BB(10,guessed_local(233860967)):
  goto __BB7(precise(134217728));

  __BB(6,guessed_local(233860966)):
  t_14 = t_12 + tt_13;
  goto __BB7(precise(134217728));

  __BB(7,guessed_local(467721933)):
  y1_5 = __PHI (__BB10: tt_13, __BB6: 0);
  t_6 = __PHI (__BB10: t_12, __BB6: t_14);
  __MEM <int[2]> (pEndpoints_15(D))[0] = t_6;
  __MEM <int[2]> (pEndpoints_15(D))[1] = y1_5;
  __MEM <int[2]> (pEndpoints_15(D) + _Literal (int *) 16)[0] = t_6;
  __MEM <int[2]> (pEndpoints_15(D) + _Literal (int *) 16)[1] = y1_5;
  goto __BB8(precise(134217728));

  __BB(8,guessed_local(1073741824)):
  return;

}


