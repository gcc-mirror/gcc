/* { dg-do compile } */
/* { dg-options "-Wdeprecated-non-prototype" } */

static inline
int f (int x)
{
  return __builtin_constant_p (x);
}

static inline
int g (double x)
{
  return __builtin_isfinite (x);
}
