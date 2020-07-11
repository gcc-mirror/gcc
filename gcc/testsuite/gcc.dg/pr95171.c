/* { dg-do compile } */
/* { dg-options "-fexceptions -ffinite-math-only -fnon-call-exceptions" } */

inline double __attribute__ ((always_inline))
w9 (int q2)
{
  return __builtin_fabs (__builtin_nan ("")) > 0.0 ? 1.0 : q2 / 1.0;
}

double __attribute__ ((optimize ("-fipa-cp")))
o7 (int iz)
{
  int rj[1];

  (void) rj;

  return w9 (iz);
}
