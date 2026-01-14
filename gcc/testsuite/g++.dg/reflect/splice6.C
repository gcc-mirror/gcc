// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

template <typename U>
constexpr int foo (int x, const U &) { return 41 + x; }
constexpr auto foo1 = ^^foo;
template <typename U>
constexpr int foo (long x, const U &) { return 42 + x; }
template <typename U>
constexpr int foo (long long x, const U &) { return 43 + x; }

static_assert (template [:foo1:] (1, 1.0f) == 42);
static_assert (template [:foo1:] (1L, 1.0) == 42);
static_assert (template [:foo1:] (1LL, 1.0L) == 42);
static_assert (template [:foo1:] <float> (1, 1.0f) == 42);
static_assert (template [:foo1:] <double> (1L, 1.0) == 42);
static_assert (template [:foo1:] <long double> (1LL, 1.0L) == 42);

template <int N>
void
bar ()
{
  static_assert (template [:foo1:] (1, 1.0f) == N);
  static_assert (template [:foo1:] (1L, 1.0) == N);
  static_assert (template [:foo1:] (1LL, 1.0L) == N);
  static_assert (template [:foo1:] <float> (1, 1.0f) == N);
  static_assert (template [:foo1:] <double> (1L, 1.0) == N);
  static_assert (template [:foo1:] <long double> (1LL, 1.0L) == N);
}

template <auto f>
void
baz ()
{
  static_assert (template [:f:] (1, 1.0f) == 42);
  static_assert (template [:f:] (1L, 1.0) == 42);
  static_assert (template [:f:] (1LL, 1.0L) == 42);
  static_assert (template [:f:] <float> (1, 1.0f) == 42);
  static_assert (template [:f:] <double> (1L, 1.0) == 42);
  static_assert (template [:f:] <long double> (1LL, 1.0L) == 42);
}

void
qux ()
{
  bar <42> ();
  baz <foo1> ();
}
