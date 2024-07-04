// DR 2627 - Bit-fields and narrowing conversions
// { dg-do compile { target c++20 } }

#include <compare>

struct C {
  long long i : 8;
};

void f() {
  C x{1}, y{2};
  x.i <=> y.i;
}
