// PR c++/49813
// { dg-options -std=c++0x }

inline constexpr bool
isinf(long double __x)
{ return __builtin_isinf(__x); }

inline constexpr bool
isinf(double __x)
{ return __builtin_isinf(__x); }

inline constexpr bool
isnan(long double __x)
{ return __builtin_isnan(__x); }

int main()
{
  constexpr long double num1 = __builtin_isinf(1.l); // Ok.

  constexpr long double num2 = isinf(1.l);           // Error.

  constexpr double      num3 = isinf(1.);            // Ok.

  constexpr long double num4 = isnan(1.l);           // Ok.
}
