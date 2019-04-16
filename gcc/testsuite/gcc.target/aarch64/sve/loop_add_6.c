/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fgimple -ffast-math" } */

double __GIMPLE (ssa, startwith("loop"))
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

 __BB(5):
  goto __BB2;

 __BB(2):
  res_1 = __PHI (__BB5: 0.0, __BB3: res_2);
  i_4 = __PHI (__BB5: 0, __BB3: i_5);
  ivtmp_6 = __PHI (__BB5: 100U, __BB3: ivtmp_7);
  index = (long unsigned int) i_4;
  offset = index * 8UL;
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

/* { dg-final { scan-assembler {\tfsub\tz[0-9]+\.d, p[0-7]/m} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
