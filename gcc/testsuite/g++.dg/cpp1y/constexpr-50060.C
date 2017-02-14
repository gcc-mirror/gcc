// PR c++/50060
// { dg-do compile { target c++14 } }

// sincos and lgamma_r aren't available in -std=c++14,
// only in -std=gnu++14.  Use __builtin_* in that case.
extern "C" void sincos (double, double *, double *);
extern "C" double frexp (double, int *);
extern "C" double modf (double, double *);
extern "C" double remquo (double, double, int *);
extern "C" double lgamma_r (double, int *);

constexpr double
f0 (double x)
{
  double y {};
  double z {};
  __builtin_sincos (x, &y, &z);
  return y;
}

constexpr double
f1 (double x)
{
  double y {};
  double z {};
  __builtin_sincos (x, &y, &z);
  return z;
}

constexpr double
f2 (double x)
{
  int y {};
  return frexp (x, &y);
}

constexpr int
f3 (double x)
{
  int y {};
  frexp (x, &y);
  return y;
}

constexpr double
f4 (double x)
{
  double y {};
  return modf (x, &y);
}

constexpr double
f5 (double x)
{
  double y {};
  modf (x, &y);
  return y;
}

constexpr double
f6 (double x, double y)
{
  int z {};
  return remquo (x, y, &z);
}

constexpr int
f7 (double x, double y)
{
  int z {};
  remquo (x, y, &z);
  return z;
}

constexpr double
f8 (double x)
{
  int y {};
  return __builtin_lgamma_r (x, &y);
}

constexpr int
f9 (double x)
{
  int y {};
  __builtin_lgamma_r (x, &y);
  return y;
}

static_assert (f0 (0.0) == 0.0, "");
static_assert (f1 (0.0) == 1.0, "");
static_assert (f2 (6.5) == 0.8125, "");
static_assert (f3 (6.5) == 3, "");
static_assert (f4 (-7.25) == -0.25, "");
static_assert (f5 (-7.25) == -7.0, "");
static_assert (f6 (3.0, 2.0) == -1.0, "");
static_assert (f7 (3.0, 2.0) == 2, "");
static_assert (f8 (0.75) >= 0.20 && f8 (0.75) <= 0.21, "");
static_assert (f8 (0.75) >= 0.20 && f8 (0.75) <= 0.21, "");
static_assert (f9 (0.75) == 1, "");
