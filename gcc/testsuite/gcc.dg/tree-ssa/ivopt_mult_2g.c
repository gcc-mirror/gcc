/* { dg-do compile { target {{ i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-O2 -fgimple -m64 -fdump-tree-ivopts-details" } */

/* Exit tests 'i < N1' and 'p2 > p_limit2' can be replaced, so
 * two ivs i and p2 can be eliminate.  */
long int __GIMPLE (startwith("fix_loops"))
foo (long int * p, long int * p2, int N1, int N2)
{
  long int s;
  long int * p_limit2;
  int i;
  long unsigned int _1;
  long unsigned int _2;
  long int _3;

  bb_2:
  _1 = (long unsigned int) N2_9(D);
  _2 = _1 * 8ul;
  p_limit2_11 = p2_10(D) + _2;
  if (N1_13(D) > 0)
    goto bb_3;
  else
    goto bb_13;

  bb_13:

  bb_9:
  goto bb_6;

  bb_3:
  p_22 = p_12(D) + 8ul;
  p2_23 = p2_10(D) + 8ul;
  if (p_limit2_11 < p2_23)
    goto bb_14;
  else
    goto bb_7;

  bb_14:
  goto bb_9;

  bb_7:
  goto bb_5;

  bb_4:
  p_14 = p_27 + 8ul;
  p2_15 = p2_28 + 8ul;
  i_16 = i_29 + 1;
  if (p_limit2_11 < p2_15)
    goto bb_11;
  else
    goto bb_8;

  bb_11:
  goto bb_6;

  bb_8:
  ;

  bb_5:
  s_25 = __PHI (bb_7: 0l, bb_8: s_18);
  p_27 = __PHI (bb_7: p_22, bb_8: p_14);
  p2_28 = __PHI (bb_7: p2_23, bb_8: p2_15);
  i_29 = __PHI (bb_7: 1, bb_8: i_16);
  _3 = __MEM <long int> (p_27);
  s_18 = _3 + s_25;
  if (N1_13(D) > i_29)
    goto bb_4;
  else
    goto bb_12;

  bb_12:
  ;

  bb_6:
  s_26 = __PHI (bb_12: s_18, bb_11: s_18, bb_9: 0l);
  return s_26;
}

/* { dg-final { scan-tree-dump-times "Replacing" 2 "ivopts"} } */
