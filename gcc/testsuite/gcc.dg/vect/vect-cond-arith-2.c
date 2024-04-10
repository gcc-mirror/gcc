/* { dg-do compile } */
/* { dg-additional-options "-fgimple -fdump-tree-optimized -ffast-math" } */
/* { dg-additional-options "-fno-vect-cost-model" { target { riscv_v } } } */

double __GIMPLE (ssa, startwith("loop"))
neg_xi (double *x)
{
  int i;
  __SIZETYPE__ index;
  __SIZETYPE__ offset;
  double * xi_ptr;
  double xi;
  double neg_xi;
  double res;
  unsigned int ivtmp;

 __BB(5):
  goto __BB2;

 __BB(2):
  res_1 = __PHI (__BB5: 0.0, __BB3: res_2);
  i_4 = __PHI (__BB5: 0, __BB3: i_5);
  ivtmp_6 = __PHI (__BB5: 100U, __BB3: ivtmp_7);
  index = (__SIZE_TYPE__) i_4;
  offset = index * _Literal(__SIZE_TYPE__) 8;
  xi_ptr = x_8(D) + offset;
  xi = *xi_ptr;
  neg_xi = -xi;
  res_2 = neg_xi + res_1;
  i_5 = i_4 + 1;
  ivtmp_7 = ivtmp_6 - 1U;
  if (ivtmp_7 != 0U)
    goto __BB3;
  else
    goto __BB4;

 __BB(3):
  goto __BB2;

 __BB(4):
  res_3 = __PHI (__BB2: res_2);
  return res_3;
}

/* { dg-final { scan-tree-dump { = \.COND_(LEN_)?ADD} "vect" { target { vect_double_cond_arith && vect_fully_masked } } } } */
/* { dg-final { scan-tree-dump { = \.COND_(LEN_)?SUB} "optimized" { target { vect_double_cond_arith && vect_fully_masked } } } } */
