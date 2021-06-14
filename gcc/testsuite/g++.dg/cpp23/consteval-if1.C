// P1938R3
// { dg-do run { target c++20 } }
// { dg-options "" }

extern "C" void abort ();

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    if consteval {	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
      return true;
    } else {
      return false;
    }
  }
}

consteval int foo (int x) { return x; }
consteval int bar () { return 2; }

constexpr int
baz (int x)
{
  int r = 0;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (x);
    }
  else
    {
      r += bar ();
    }
  if ! consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 2 * bar ();
    }
  else
    {
      r += foo (8 * x);
    }
  if (std::is_constant_evaluated ())
    r = -r;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (32 * x);
    }
  if not consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 32 * bar ();
    }
  return r;
}

template <typename T>
constexpr int
qux (T x)
{
  T r = 0;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (x);
    }
  else
    {
      r += bar ();
    }
  if ! consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 2 * bar ();
    }
  else
    {
      r += foo (8 * x);
    }
  if (std::is_constant_evaluated ())
    r = -r;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (32 * x);
    }
  if not consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 32 * bar ();
    }
  return r;
}

constexpr int a = baz (1);
static_assert (a == 23);
int b = baz (1);
constexpr int c = qux (1);
static_assert (c == 23);
int d = qux<int> (1);

int
main ()
{
  if (b != 23 || d != 23)
    abort ();
  if (baz (1) != 70 || qux (1) != 70 || qux (1LL) != 70)
    abort ();
}
