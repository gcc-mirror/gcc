/* PR rtl-optimization/64935 */
/* { dg-do compile } */
/* { dg-options "-O -fschedule-insns --param=max-sched-ready-insns=0 -fcompare-debug" } */
/* { dg-require-effective-target scheduling } */
/* { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } } */

void
foo (int *data, unsigned len, const int qlp_coeff[],
     unsigned order, int lp, int residual[], int i)
{
  int sum;
  sum = 0;
  sum += qlp_coeff[1] * data[i - 2];
  sum += qlp_coeff[0] * data[i - 1];
  residual[i] = data[i] - (sum >> lp);
}
