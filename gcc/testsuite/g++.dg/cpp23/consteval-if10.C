// P1938R3
// { dg-do compile { target c++20 } }
// { dg-options "" }

consteval int foo (int x) { return x; }

constexpr int
bar (int x)
{
  int r = 0;
  if consteval		// { dg-warning "'if consteval' only available with" "" { target c++20_only } }
    {
      auto y = [=] { foo (x); };	// { dg-error "'x' is not a constant expression" }
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
      auto y = [=] { foo (x); };	// { dg-error "'x' is not a constant expression" }
      y ();
    }
  return r;
}

int
qux (int x)
{
  return baz (x);
}
