/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl -fno-vect-cost-model" } */

int foo (double g, int f, double *r, int *s)
{
  int hu = 0;
  bool test0 = r[0] < g;
  bool test1 = r[1] < g;
  bool test2 = r[2] < g;
  bool test3 = r[3] < g;
  bool test4 = s[0] < f;
  bool test5 = s[1] < f;
  bool test6 = s[2] < f;
  bool test7 = s[3] < f;
  hu += (test0 & test4) + (test1 & test5) + (test2 & test6) + (test3 & test7);
  return hu;
}

/* { dg-final { scan-assembler "vcmppd" } } */
/* { dg-final { scan-assembler "vpcmpd" } } */
