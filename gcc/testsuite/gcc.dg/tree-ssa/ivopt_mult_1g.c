/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fgimple -m64 -fdump-tree-ivopts-details" } */

/* The test 'if (p2 > p_limit2)' can be replaced, so iv p2 can be
 * eliminated.  */
long int __GIMPLE (startwith("fix_loops"))
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

  bb_2:
  _1 = (long unsigned int) N1_10(D);
  _2 = _1 * 8ul;
  p_limit_12 = p_11(D) + _2;
  _3 = (long unsigned int) N2_13(D);
  _4 = _3 * 8ul;
  p_limit2_15 = p2_14(D) + _4;
  if (p_11(D) <= p_limit_12)
    goto bb_3;
  else
    goto bb_13;

  bb_13:

  bb_9:
  goto bb_6;

  bb_3:
  p_20 = p_11(D) + 8ul;
  p2_23 = p2_14(D) + 8ul;
  if (p_limit2_15 < p2_23)
    goto bb_14;
  else
    goto bb_7;

  bb_14:
  goto bb_9;

  bb_7:
  goto bb_5;

  bb_4:
  p_16 = p_26 + 8ul;
  p2_17 = p2_27 + 8ul;
  if (p_limit2_15 < p2_17)
    goto bb_11;
  else
    goto bb_8;

  bb_11:
  goto bb_6;

  bb_8:
  ;

  bb_5:
  s_24 = __PHI (bb_7: 0l, bb_8: s_19);
  p_26 = __PHI (bb_7: p_20, bb_8: p_16);
  p2_27 = __PHI (bb_7: p2_23, bb_8: p2_17);
  _5 = __MEM <long int> (p_26);
  s_19 = _5 + s_24;
  if (p_limit_12 >= p_26)
    goto bb_4;
  else
    goto bb_12;

  bb_12:
  ;

  bb_6:
  s_25 = __PHI (bb_12: s_19, bb_11: s_19, bb_9: 0l);
  return s_25;
}

/* { dg-final { scan-tree-dump-times "Replacing" 1 "ivopts" } } */
