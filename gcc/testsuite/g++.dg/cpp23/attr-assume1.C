// P1774R8 - Portable assumptions
// { dg-do run { target c++11 } }

namespace std
{
  constexpr bool
  isfinite (float x)
  { return __builtin_isfinite (x); }

  constexpr bool
  isfinite (double x)
  { return __builtin_isfinite (x); }

  constexpr bool
  isfinite (long double x)
  { return __builtin_isfinite (x); }

  constexpr float
  sqrt (float x)
  { return __builtin_sqrtf (x); }

  constexpr double
  sqrt (double x)
  { return __builtin_sqrt (x); }

  constexpr long double
  sqrt (long double x)
  { return __builtin_sqrtl (x); }

  extern "C" void
  abort ();
}

constexpr int
f1 (int i)
{
#if __cpp_constexpr >= 201603L
  auto f = [=] { [[assume (i == 0)]]; };
  return sizeof (f);
#else
  return sizeof (int);
#endif
}

void
f2 ()
{
  static_assert (f1 (0) >= sizeof (int), "");
}

int
f3 (int i)
{
  [[assume (i == 42)]];
  return i;
}

int
f4 (int i)
{
  [[assume (++i == 44)]];
  return i;
}

int a;
int *volatile c;

bool
f5 ()
{
  ++a;
  return true;
}

constexpr int
f6 ()
{
#if __cpp_constexpr >= 201304L
  [[assume (f5 ())]];
#endif
  return 1;
}

template <int ...args>
bool
f7 ()
{
#if __cpp_fold_expressions >= 201411L
  [[assume (((args >= 0) && ...))]];
  return ((args >= 0) && ...);
#else
  return true;
#endif
}

bool
f8 (double x)
{
  [[assume (std::isfinite (x) && x >= 0.0)]];
  return std::isfinite (std::sqrt (x));
}

double
f9 (double x)
{
  [[assume (std::isfinite (std::sqrt (x)))]];
  return std::sqrt (x);
}

template <typename T, T N>
T
f10 (T x)
{
  [[assume (x == N)]];
  return x;
}

int
f11 (int x)
{
  [[assume (x == 93 ? true : throw 1)]];
  return x;
}

constexpr int
f12 (int x)
{
#if __cpp_constexpr >= 201304L
  [[assume (++x == 43)]];
#endif
  return x;
}

static_assert (f12 (42) == 42, "");

struct S
{
  operator bool () { return true; }
};

int
f13 ()
{
  S s;
  [[assume (s)]];
  return 0;
}

template <typename T>
int
f14 ()
{
  T t;
  [[assume (t)]];
  return 0;
}

int
main ()
{
  int b = 42;
  double d = 42.0, e = 43.0;
  c = &b;
  [[assume (f5 ())]];
  if (a)
    std::abort ();
  [[assume (++b == 43)]];
  if (b != 42 || *c != 42)
    std::abort ();
  static_assert (f6 () == 1, "");
  if (f6 () != 1)
    std::abort ();
  if (a)
    std::abort ();
  if (!f7 <0> () || !f7 <1, 2, 3, 4> ())
    std::abort ();
  [[assume (d < e)]];
  if (f10 <int, 45> (45) != 45
      || f10 <long long, 128LL> (128LL) != 128LL
#if __cpp_nontype_template_args >= 201911L
      || f10 <long double, -42.0L> (-42.0L) != -42.0L
#endif
      || false)
    std::abort ();
  int i = 90, j = 91, k = 92;
  [[assume (i == 90), assume (j <= 91)]] [[assume (k >= 92)]];
  if (f11 (93) != 93)
    std::abort ();
  if (f14 <S> () != 0)
    std::abort ();
}
