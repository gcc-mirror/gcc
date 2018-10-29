// { dg-do compile { target c++17 } }

auto ID = [] (int n) constexpr { return n; };
constexpr int I = ID(3);
