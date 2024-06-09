// P2564R3
// { dg-do compile { target c++20 } }

consteval int
zero (int)
{
  return 0;
}

constexpr int
f1 (auto i)
{
  return zero (i);
}

constexpr int
f2 (auto i)
{
  return f1 (i);
}

constexpr int
f3 (auto i)
{
  return f2 (i);
}

constexpr int
f4 (auto i)
{
  return f3 (i);
}

constexpr int
f5 (auto i)
{
  return f4 (i);
}

constexpr int
f6 (auto)
{
  // This call is a constant expression, so don't promote f6.
  return f5 (42);
}

constexpr int
f7 (auto)
{
  // This call is a constant expression, so don't promote f7.
  return zero (42);
}

auto p1 = &f5<int>; // { dg-error "taking address" }
static auto p2 = &f4<int>; // { dg-error "taking address" }
auto p3 = &f6<int>;
static auto p4 = &f6<int>;
auto p5 = &f7<int>;
static auto p6 = &f7<int>;

void
g ()
{
  static auto q1 = &f4<int>; // { dg-error "taking address" }
  static auto q2 = &f6<int>;
  static auto q3 = &f7<int>;
}
