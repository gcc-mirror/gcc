// PR target/88152
// { dg-do compile }
// { dg-options "-O2 -std=c++14 -fdump-tree-forwprop1" }
// { dg-final { scan-tree-dump-times " (?:<|>=) \{ 0\[, ]" 120 "forwprop1" } }

template <typename T, int N>
using V [[gnu::vector_size (sizeof (T) * N)]] = T;

void *foo ();

template <typename T, int N, T max, T maxp1>
__attribute__((noipa)) void
test_uns ()
{
  V<T, N> *x = (V<T, N> *) foo ();
  x[1] = x[0] > max;
  x[3] = x[2] >= maxp1;
  x[5] = x[4] <= max;
  x[7] = x[6] < maxp1;
}

template <typename T, int N>
__attribute__((noipa)) void
test ()
{
  V<T, N> *x = (V<T, N> *) foo ();
  x[1] = x[0] >= 0;
  x[3] = x[2] > -1;
  x[5] = x[4] < 0;
  x[7] = x[6] <= -1;
}

template <int N>
__attribute__((noipa)) void
tests ()
{
  test_uns<unsigned char, N, __SCHAR_MAX__, 1U + __SCHAR_MAX__> ();
  test<signed char, N> ();
  test_uns<unsigned short int, N, __SHRT_MAX__, 1U + __SHRT_MAX__> ();
  test<short int, N> ();
  test_uns<unsigned int, N, __INT_MAX__, 1U + __INT_MAX__> ();
  test<int, N> ();
  test_uns<unsigned long int, N, __LONG_MAX__, 1UL + __LONG_MAX__> ();
  test<long int, N> ();
  test_uns<unsigned long long int, N, __LONG_LONG_MAX__, 1ULL + __LONG_LONG_MAX__> ();
  test<long long int, N> ();
}

void
all_tests ()
{
  tests<1> ();
  tests<2> ();
  tests<8> ();
}
