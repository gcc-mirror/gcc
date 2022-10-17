// P1938R3
// { dg-do compile { target c++20 } }
// { dg-options "" }

constexpr bool f()
{
  if consteval (true) {}	// { dg-error "'if consteval' requires compound statement" }
				// { dg-error "expected" "" { target *-*-* } .-1 }
				// { dg-warning "'if consteval' only available with" "" { target c++20_only } .-2 }
  if not consteval (false) {}	// { dg-error "'if consteval' requires compound statement" }
				// { dg-error "expected" "" { target *-*-* } .-1 }
				// { dg-warning "'if consteval' only available with" "" { target c++20_only } .-2 }
  if consteval if (true) {}	// { dg-error "'if consteval' requires compound statement" }
				// { dg-warning "'if consteval' only available with" "" { target c++20_only } .-1 }
  if ! consteval {} else ;	// { dg-error "'if consteval' requires compound statement" }
				// { dg-warning "'if consteval' only available with" "" { target c++20_only } .-1 }
  if consteval {} else if (true) {}	// { dg-error "'if consteval' requires compound statement" }
				// { dg-warning "'if consteval' only available with" "" { target c++20_only } .-1 }
  if (true)
    if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
      {
      }
    else ;			// { dg-error "'if consteval' requires compound statement" }
  return false;
}

consteval int foo (int x) { return x; }
consteval int bar () { return 2; }

constexpr int
baz (int x)
{
  int r = 0;
  if not consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (x);	// { dg-error "'x' is not a constant expression" }
    }
  else
    {
      r += bar ();
    }
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 2 * bar ();
    }
  else
    {
      r += foo (8 * x);	// { dg-error "'x' is not a constant expression" }
    }
  if ! consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (32 * x);// { dg-error "'x' is not a constant expression" }
    }
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 32 * bar ();
    }
  return r;
}

template <typename T>
constexpr int
qux (int x)
{
  int r = 0;
  if not consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (x);	// { dg-error "'x' is not a constant expression" }
    }
  else
    {
      r += bar ();
    }
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 2 * bar ();
    }
  else
    {
      r += foo (8 * x);	// { dg-error "is not a constant expression" "" { xfail *-*-* } }
    }
  if ! consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (32 * x);// { dg-error "is not a constant expression" "" { xfail *-*-* } }
    }
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 32 * bar ();
    }
  return r;
}

template <typename T>
constexpr T
corge (T x)
{
  T r = 0;
  if not consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (x);	// { dg-error "'x' is not a constant expression" }
    }
  else
    {
      r += bar ();
    }
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 2 * bar ();
    }
  else
    {
      r += foo (8 * x);	// { dg-error "is not a constant expression" }
    }
  if ! consteval	// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += foo (32 * x);// { dg-error "is not a constant expression" }
    }
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      r += 32 * bar ();
    }
  return r;
}

int
garply (int x)
{
  return corge (x);
}
