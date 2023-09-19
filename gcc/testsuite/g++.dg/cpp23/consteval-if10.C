// P1938R3
// { dg-do compile { target c++20 } }
// { dg-options "" }

// We used to give errors but the lambdas are now promoted to consteval
// and are in a immediate function context, so no errors.

consteval int foo (int x) { return x; }

constexpr int
bar (int x)
{
  int r = 0;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      auto y = [=] { foo (x); };
      y ();
    }
  return r;
}

template <typename T>
constexpr T
baz (T x)
{
  T r = 0;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      auto y = [=] { foo (x); };
      y ();
    }
  return r;
}

int
qux (int x)
{
  return baz (x);
}
