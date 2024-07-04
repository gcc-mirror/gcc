// { dg-do compile { target c++14 } }

template <int N>
void
foo (long double *ptr, long double *val, long double *ret)
{
  __atomic_exchange (ptr, val, ret, __ATOMIC_RELAXED);
}

template <int N>
bool
bar (long double *ptr, long double *exp, long double *des)
{
  return __atomic_compare_exchange (ptr, exp, des, false,
				    __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

bool
baz (long double *p, long double *q, long double *r)
{
  foo<0> (p, q, r);
  foo<1> (p + 1, q + 1, r + 1);
  return bar<0> (p + 2, q + 2, r + 2) || bar<1> (p + 3, q + 3, r + 3);
}

constexpr int
qux (long double *ptr, long double *val, long double *ret)
{
  __atomic_exchange (ptr, val, ret, __ATOMIC_RELAXED);
  return 0;
}

constexpr bool
corge (long double *ptr, long double *exp, long double *des)
{
  return __atomic_compare_exchange (ptr, exp, des, false,
				    __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

long double a[6];
const int b = qux (a, a + 1, a + 2);
const bool c = corge (a + 3, a + 4, a + 5);
