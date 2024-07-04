// P2564R3
// { dg-do compile { target c++20 } }

consteval int id(int i) { return i; }

constexpr int
f (auto t)
{
  return t + id (t);
}

constexpr int
f2 (auto t)
{
  return t + f(t); // { dg-message "immediate-escalating expression .f<int>\\(t\\)." }
}

int z; // { dg-message "not const" }
auto y1 = f2 (42);
auto y2 = f2 (z); // { dg-error "value of .z. is not usable in a constant expression|call to consteval function" }
