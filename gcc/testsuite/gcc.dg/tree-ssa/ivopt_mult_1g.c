/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fgimple -m64 -fdump-tree-ivopts-details" } */

/* The test 'if (p2 > p_limit2)' can be replaced, so iv p2 can be
 * eliminated.  */
long int __GIMPLE (ssa,startwith("fix_loops"))
foo (long int * p, long int * p2, int N1, int N2)
{
  long int s;
  long int * p_limit2;
  long int * p_limit;
  long unsigned int _1;
  long unsigned int _2;
  long unsigned int _3;
  long unsigned int _4;
  long int _5;

  __BB(2):
  _1 = (long unsigned int) N1_10(D);
  _2 = _1 * 8ul;
  p_limit_12 = p_11(D) + _2;
  _3 = (long unsigned int) N2_13(D);
  _4 = _3 * 8ul;
  p_limit2_15 = p2_14(D) + _4;
  if (p_11(D) <= p_limit_12)
    goto __BB3;
  else
    goto __BB13;

  __BB(13):
  goto __BB9;

  __BB(9):
  goto __BB6;

  __BB(3):
  p_20 = p_11(D) + 8ul;
  p2_23 = p2_14(D) + 8ul;
  if (p_limit2_15 < p2_23)
    goto __BB14;
  else
    goto __BB7;

  __BB(14):
  goto __BB9;

  __BB(7):
  goto __BB5;

  __BB(4):
  p_16 = p_26 + 8ul;
  p2_17 = p2_27 + 8ul;
  if (p_limit2_15 < p2_17)
    goto __BB11;
  else
    goto __BB8;

  __BB(11):
  goto __BB6;

  __BB(8):
  goto __BB5;

  __BB(5):
  s_24 = __PHI (__BB7: 0l, __BB8: s_19);
  p_26 = __PHI (__BB7: p_20, __BB8: p_16);
  p2_27 = __PHI (__BB7: p2_23, __BB8: p2_17);
  _5 = __MEM <long int> (p_26);
  s_19 = _5 + s_24;
  if (p_limit_12 >= p_26)
    goto __BB4;
  else
    goto __BB12;

  __BB(12):
  goto __BB6;

  __BB(6):
  s_25 = __PHI (__BB12: s_19, __BB11: s_19, __BB9: 0l);
  return s_25;
}

/* { dg-final { scan-tree-dump-times "Replacing" 1 "ivopts" } } */
