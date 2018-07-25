/* { dg-do compile } */
/* { dg-additional-options "-fgimple -fdump-tree-optimized -ffast-math" } */

double __GIMPLE (startwith("loop"))
neg_xi (double *x)
{
  int i;
  long unsigned int index;
  long unsigned int offset;
  double * xi_ptr;
  double xi;
  double neg_xi;
  double res;
  unsigned int ivtmp;

 bb_1:
  goto bb_2;

 bb_2:
  res_1 = __PHI (bb_1: 0.0, bb_3: res_2);
  i_4 = __PHI (bb_1: 0, bb_3: i_5);
  ivtmp_6 = __PHI (bb_1: 100U, bb_3: ivtmp_7);
  index = (long unsigned int) i_4;
  offset = index * 8UL;
  xi_ptr = x_8(D) + offset;
  xi = *xi_ptr;
  neg_xi = -xi;
  res_2 = neg_xi + res_1;
  i_5 = i_4 + 1;
  ivtmp_7 = ivtmp_6 - 1U;
  if (ivtmp_7 != 0U)
    goto bb_3;
  else
    goto bb_4;

 bb_3:
  goto bb_2;

 bb_4:
  res_3 = __PHI (bb_2: res_2);
  return res_3;
}

/* { dg-final { scan-tree-dump { = \.COND_ADD} "vect" { target { vect_double_cond_arith && vect_fully_masked } } } } */
/* { dg-final { scan-tree-dump { = \.COND_SUB} "optimized" { target { vect_double_cond_arith && vect_fully_masked } } } } */
