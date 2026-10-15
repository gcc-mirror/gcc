/* { dg-do assemble } */
/* { dg-require-effective-target lto } */
/* { dg-options "-O2 -flto" } */

/* LTO should record library references for all pi-based builtins.  */

extern double acospi (double);
extern double asinpi (double);
extern double atan2pi (double, double);
extern double atanpi (double);
extern double cospi (double);
extern double sinpi (double);
extern double tanpi (double);

double
f1 (double x)
{
  return acospi (x);
}

double
f2 (double x)
{
  return asinpi (x);
}

double
f3 (double x, double y)
{
  return atan2pi (x, y);
}

double
f4 (double x)
{
  return atanpi (x);
}

double
f5 (double x)
{
  return cospi (x);
}

double
f6 (double x)
{
  return sinpi (x);
}

double
f7 (double x)
{
  return tanpi (x);
}

/* { dg-final { scan-symbol "acospi" } } */
/* { dg-final { scan-symbol "asinpi" } } */
/* { dg-final { scan-symbol "atan2pi" } } */
/* { dg-final { scan-symbol "atanpi" } } */
/* { dg-final { scan-symbol "cospi" } } */
/* { dg-final { scan-symbol "sinpi" } } */
/* { dg-final { scan-symbol "tanpi" } } */
