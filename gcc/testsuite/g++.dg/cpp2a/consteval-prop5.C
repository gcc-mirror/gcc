// P2564R3
// { dg-do compile { target c++20 } }

consteval int f (int i) { return i; }

struct S {
  int x = f(42);
};

constexpr S
immediate (auto)
{
  return S{};
}

void
g ()
{
  immediate (0);
}

consteval void
test ()
{
  constexpr S s = immediate(0);
  static_assert(s.x == 42);
}
