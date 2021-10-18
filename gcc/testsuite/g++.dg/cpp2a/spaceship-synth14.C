// Test virtual <=>.
// { dg-do run { target c++20 } }

#include <compare>

struct E;
struct D {
  std::partial_ordering operator<=>(const D&) const = default;
  virtual std::partial_ordering operator<=>(const E&) const = 0;
  float f;
  D(float f): f(f) {}
};
struct E : D {
  std::partial_ordering operator<=>(const E&) const override = default;
  int i;
  E(float f, int i): D(f), i(i) {}
};

int main()
{
  E e1{0.0,42};
  E e2{1.0,24};
  auto a = e1 <=> e2;
  if (!is_lt (e1 <=> e2))
    __builtin_abort();
}
