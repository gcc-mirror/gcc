// PR c++/78334
// { dg-do compile { target c++17 } }

template <auto> auto constexpr_string([](auto) {});
void foo() { constexpr_string<0>(0); }
