// PR c++/88174
// { dg-do compile { target c++14 } }

constexpr bool
foo (double x, double y, double z, double w)
{
  __complex__ double a = 0;
  __real__ a = x;
  __imag__ a = y;
#if __cpp_constexpr >= 201907L
  __complex__ double b;
  __real__ b = z;
#else
  __complex__ double b = z;
#endif
  __imag__ b = w;
  a += b;
  a -= b;
  a *= b;
  a /= b;
  return __real__ a == x && __imag__ a == y;
}

static_assert (foo (1.0, 2.0, 3.0, 4.0), "");
