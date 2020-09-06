// PR c++/77841
// { dg-do compile { target c++11 } }

auto p1 = new int[][1]();
auto p2 = new int[1][1]();
#if __cpp_aggregate_paren_init
auto p3 = new int[][4]({1, 2}, {3, 4});
auto p4 = new int[2][4]({1, 2}, {3, 4});
auto p5 = new int[2][1]({1, 2}, {3}); // { dg-error "too many initializers" "" { target c++20 } }
#endif

auto b1 = new int[][1]{};
auto b2 = new int[1][1]{};
auto b3 = new int[][4]{{1, 2}, {3, 4}};
auto b4 = new int[2][4]{{1, 2}, {3, 4}};
