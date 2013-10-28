// { dg-options -std=c++11 }

struct Empty {};

constexpr bool f(Empty) { return true; }

constexpr bool x(f(Empty{}));
