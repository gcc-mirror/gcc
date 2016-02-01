// PR rtl-optimization/69570
// { dg-do run }
// { dg-options "-O2" }
// { dg-additional-options "-fpic" { target fpic } }
// { dg-additional-options "-march=i686" { target ia32 } }

template <typename T> inline const T &
min (const T &a, const T &b)
{
  if (b < a)
    return b;
  return a;
}

template <typename T> inline const T &
max (const T &a, const T &b)
{
  if (a < b)
    return b;
  return a;
}

static inline void
foo (unsigned x, unsigned y, unsigned z, double &h, double &s, double &l)
{
  double r = x / 255.0;
  double g = y / 255.0;
  double b = z / 255.0;
  double m = max (r, max (g, b));
  double n = min (r, min (g, b));
  double d = m - n;
  double e = m + n;
  h = 0.0, s = 0.0, l = e / 2.0;
  if (d > 0.0)
    {
      s = l > 0.5 ? d / (2.0 - e) : d / e;
      if (m == r && m != g)
        h = (g - b) / d + (g < b ? 6.0 : 0.0);
      if (m == g && m != b)
        h = (b - r) / d + 2.0;
      if (m == b && m != r)
        h = (r - g) / d + 4.0;
      h /= 6.0;
    }
}

__attribute__ ((noinline, noclone))
void bar (unsigned x[3], double y[3])
{
  double h, s, l;
  foo (x[0], x[1], x[2], h, s, l);
  y[0] = h;
  y[1] = s;
  y[2] = l;
}

int
main ()
{
  unsigned x[3] = { 0, 128, 0 };
  double y[3];

  bar (x, y);
  if (__builtin_fabs (y[0] - 0.33333) > 0.001
      || __builtin_fabs (y[1] - 1) > 0.001
      || __builtin_fabs (y[2] - 0.25098) > 0.001)
    __builtin_abort ();

  return 0;
}
