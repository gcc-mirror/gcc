// { dg-options -std=c++17 }

auto ID = [] (int n) constexpr { return n; };
constexpr int I = ID(3);
