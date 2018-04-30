// PR c++/78334
// { dg-options -std=c++17 }

template <auto> auto constexpr_string([](auto) {});
void foo() { constexpr_string<0>(0); };
