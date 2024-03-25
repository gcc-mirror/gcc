/* { dg-do compile } */
/* { dg-options "-fpermissive -O2 -w" } */

extern void sincos(double x, double *sinx, double *cosx);
void apply(void (*f)(double, double *, double *),
	   double x, double *sinx, double *cosx)
{
  f(x, sinx, cosx);
  return;
}
void apply_sincos(double x, double **sinx, double **cosx)
{
  apply(sincos, x, sinx, cosx);
  return;
}
