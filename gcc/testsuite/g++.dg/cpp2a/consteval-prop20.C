// P2564R3
// { dg-do compile { target c++20 } }
// { dg-options "-Wno-c++23-extensions" }

consteval int id(int i) { return i; }

constexpr int
f (auto i)
{
  return id (i);
}

void
g ()
{
  auto p = &f<int>; // { dg-error "taking address" }
  decltype(&f<int>) x;
  if consteval {
    auto q = &f<int>;
  }
}
