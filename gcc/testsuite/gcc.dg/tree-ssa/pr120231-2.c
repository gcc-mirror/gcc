/* PR tree-optimization/120231 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64 } */
/* { dg-final { scan-tree-dump-not "link_failure \\\(\\\);" "optimized" } } */

void link_failure (void);

static _Float64 __attribute__((noinline))
f1 (signed char x)
{
  return x;
}

static _Float64 __attribute__((noinline))
f2 (signed char x)
{
  if (x >= -37 && x <= 42)
    return x;
  return 0.0f64;
}

void
f3 (signed char x)
{
  _Float64 y = f1 (x);
  if (y < (_Float64) (-__SCHAR_MAX__ - 1) || y > (_Float64) __SCHAR_MAX__)
    link_failure ();
  y = f2 (x);
  if (y < -37.0f64 || y > 42.0f64)
    link_failure ();
}

static _Float64 __attribute__((noinline))
f4 (long long x)
{
  return x;
}

static _Float64 __attribute__((noinline))
f5 (long long x)
{
  if (x >= -0x3ffffffffffffffeLL && x <= 0x3ffffffffffffffeLL)
    return x;
  return 0.0f64;
}

void
f6 (long long x)
{
  _Float64 y = f4 (x);
  if (y < (_Float64) (-__LONG_LONG_MAX__ - 1) || y > (_Float64) __LONG_LONG_MAX__)
    link_failure ();
  y = f5 (x);
  if (y < (_Float64) -0x3ffffffffffffffeLL || y > (_Float64) 0x3ffffffffffffffeLL)
    link_failure ();
}

static signed char __attribute__((noinline))
f7 (_Float64 x)
{
  if (x >= -78.5f64 && x <= 98.25f64)
    return x;
  return 0;
}

static unsigned char __attribute__((noinline))
f8 (_Float64 x)
{
  if (x >= -0.75f64 && x <= 231.625f64)
    return x;
  return 31;
}

static long long __attribute__((noinline))
f9 (_Float64 x)
{
  if (x >= -3372587051122780362.75f64 && x <= 3955322825938799366.25f64)
    return x;
  return 0;
}

static unsigned long long __attribute__((noinline))
f10 (_Float64 x)
{
  if (x >= 31.25f64 && x <= 16751991430751148048.125f64)
    return x;
  return 4700;
}

void
f11 (_Float64 x)
{
  signed char a = f7 (x);
  if (a < -78 || a > 98)
    link_failure ();
  unsigned char b = f8 (x);
  if (b > 231)
    link_failure ();
  long long c = f9 (x);
  if (c < -3372587051122780160LL || c > 3955322825938799616LL)
    link_failure ();
  unsigned long long d = f10 (x);
  if (d < 31 || d > 16751991430751148032ULL)
    link_failure ();
}
