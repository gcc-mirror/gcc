// { dg-options -std=c++0x }
int x;
constexpr int& rx = x; // { dg-error "int&" }
