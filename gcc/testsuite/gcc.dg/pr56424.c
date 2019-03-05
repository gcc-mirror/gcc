/* PR tree-optimization/56424 */

/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions -fnon-call-exceptions" } */
/* { dg-require-effective-target exceptions } */

extern long double cosl (long double);
extern long double sinl (long double);
extern long double reml (long double, long double);

long double my_cos (long double arg)
{
  return cosl (arg);
}

long double my_sin (long double arg)
{
  if (__builtin_fabs (arg) < 1.0)
    return arg;

  return sinl (arg);
}

long double my_cot (long double arg, long double cycle)
{
  long double t = reml (arg, cycle);
  return my_cos (t) / my_sin (t);
}

long double my_tan (long double arg, long double cycle)
{
  long double t = reml (arg, cycle);
  return my_sin (t) / my_cos (t);
}
