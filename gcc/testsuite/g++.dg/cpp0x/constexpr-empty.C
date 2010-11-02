// { dg-options -std=c++0x }

struct Empty {};

constexpr bool f(Empty) { return true; }

constexpr bool x(f(Empty{}));
