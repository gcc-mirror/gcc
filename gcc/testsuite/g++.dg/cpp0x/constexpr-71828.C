// PR c++/71828
// { dg-do compile { target c++11 } }

constexpr _Complex int a { 1, 2 };
static_assert (& __imag a != &__real a, "");
