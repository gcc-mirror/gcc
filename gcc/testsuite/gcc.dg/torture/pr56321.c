/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

void foo(int n, int nreps, float tdgefa, float tdgesl)
{
  float kflops,ops;
  ops=((2.0*n*n*n)/3.0+2.0*n*n);
  kflops=2.*nreps*ops/(1000.*(tdgefa+tdgesl));

  __builtin_printf ("%f\n", kflops);
}
