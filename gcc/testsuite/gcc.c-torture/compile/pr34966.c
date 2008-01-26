extern double sin (double), cos (double);

__inline double
atan (double __x)
{
  register double __result;
#if defined(__i386__) || defined(__x86_64__)
  __asm __volatile__ ("" : "=t" (__result) : "0" (__x));
#else
  __result = __x;
#endif
  return __result;
}

double
f(double x)
{
  double t = atan (x);
  return cos (t) + sin (t);
}

