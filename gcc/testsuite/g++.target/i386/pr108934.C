// PR c++/108934
// { dg-do compile { target c++11 } }

struct S { unsigned long long a[2]; };
struct T { unsigned long long b[6]; };
struct U { unsigned long long c[2]; long double d; unsigned long long e[2]; };

#if __SIZEOF_LONG_DOUBLE__ == 16 && __LDBL_MANT_DIG__ == 64 && __SIZEOF_LONG_LONG__ == 8
constexpr long double
foo (S x)
{
  return __builtin_bit_cast (long double, x);
}

constexpr S a = { 0ULL, 0xffffffffffff0000ULL };
constexpr long double b = foo (a);
static_assert (b == 0.0L, "");

constexpr U
bar (T x)
{
  return __builtin_bit_cast (U, x);
}

constexpr T c = { 0ULL, 0ULL, 0ULL, 0xffffffffffff0000ULL, 0ULL, 0ULL };
constexpr U d = bar (c);
static_assert (d.d == 0.0L, "");
#endif
