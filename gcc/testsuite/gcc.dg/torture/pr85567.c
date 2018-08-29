/* { dg-do compile } */

extern void sincos(double x, double *sinx, double *cosx);

void apply(void (*f)(double, double *, double *),
	   double x, double *sinx, double *cosx)
{
  f(x, sinx, cosx);
  return;
}

void apply_sincos(double x, double *sinx, double *cosx)
{
  apply(sincos, x, sinx, cosx);
  return;
}
