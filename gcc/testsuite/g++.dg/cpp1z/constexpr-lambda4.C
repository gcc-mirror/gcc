// { dg-options -std=c++1z }

auto ID = [] (int n) constexpr { return n; };
constexpr int I = ID(3);
