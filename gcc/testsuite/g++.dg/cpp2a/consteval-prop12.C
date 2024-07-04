// P2564R3
// { dg-do compile { target c++20 } }

consteval int
zero (int)
{
  return 0;
}

constexpr int
f (auto i)
{
  return zero (i);
}

constexpr int
g (auto)
{
  // This call is a constant expression, so don't promote g.
  return f (42);
}

void
do_test ()
{
  g (2);
}

// Must work.
auto q = &g<int>;
