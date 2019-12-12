/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fgimple -m64 -fdump-tree-ivopts-details" } */

/* Exit tests 'i < N1' and 'p2 > p_limit2' can be replaced, so
 * two ivs i and p2 can be eliminate.  */
long int __GIMPLE (ssa,startwith("fix_loops"))
foo (long int * p, long int * p2, int N1, int N2)
{
  long int s;
  long int * p_limit2;
  int i;
  long unsigned int _1;
  long unsigned int _2;
  long int _3;

  __BB(2):
  _1 = (long unsigned int) N2_9(D);
  _2 = _1 * 8ul;
  p_limit2_11 = p2_10(D) + _2;
  if (N1_13(D) > 0)
    goto __BB3;
  else
    goto __BB13;

  __BB(13):
  goto __BB9;

  __BB(9):
  goto __BB6;

  __BB(3):
  p_22 = p_12(D) + 8ul;
  p2_23 = p2_10(D) + 8ul;
  if (p_limit2_11 < p2_23)
    goto __BB14;
  else
    goto __BB7;

  __BB(14):
  goto __BB9;

  __BB(7):
  goto __BB5;

  __BB(4):
  p_14 = p_27 + 8ul;
  p2_15 = p2_28 + 8ul;
  i_16 = i_29 + 1;
  if (p_limit2_11 < p2_15)
    goto __BB11;
  else
    goto __BB8;

  __BB(11):
  goto __BB6;

  __BB(8):
  goto __BB5;

  __BB(5):
  s_25 = __PHI (__BB7: 0l, __BB8: s_18);
  p_27 = __PHI (__BB7: p_22, __BB8: p_14);
  p2_28 = __PHI (__BB7: p2_23, __BB8: p2_15);
  i_29 = __PHI (__BB7: 1, __BB8: i_16);
  _3 = __MEM <long int> (p_27);
  s_18 = _3 + s_25;
  if (N1_13(D) > i_29)
    goto __BB4;
  else
    goto __BB12;

  __BB(12):
  goto __BB6;

  __BB(6):
  s_26 = __PHI (__BB12: s_18, __BB11: s_18, __BB9: 0l);
  return s_26;
}

/* { dg-final { scan-tree-dump-times "Replacing" 2 "ivopts"} } */
