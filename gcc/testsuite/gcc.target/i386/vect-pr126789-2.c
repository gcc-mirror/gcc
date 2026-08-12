/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl -fno-vect-cost-model" } */

int foo (double g, int f, double *r, int *s)
{
  int hu = 0;
  bool test0 = r[0] < g;
  bool test1 = r[1] < g;
  bool test2 = s[0] < f;
  bool test3 = s[1] < f;
  hu += (test0 & test2) + (test1 & test3);
  return hu;
}

/* { dg-final { scan-assembler "vcmppd" } } */
/* That we use vpcmpgtd and not vpcmpd is because ix86_get_mask_mode
   does not get us QImode for MMX modes.  But we should be able to
   inter-operate with mixed SSE/AVX512 masks and vectorize the reduction.  */
/* { dg-final { scan-assembler "vpcmpgtd" } } */
