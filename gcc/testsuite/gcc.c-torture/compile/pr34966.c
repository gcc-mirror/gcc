extern double sin (double), cos (double);

__inline double
atan (double __x)
{
  register double __result;
  __asm __volatile__ ("" : "=t" (__result) : "0" (__x));
  return __result;
}

double
f(double x)
{
  double t = atan (x);
  return cos (t) + sin (t);
}

