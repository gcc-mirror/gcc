// { dg-do compile { target c++11 } }

struct Empty {};

constexpr bool f(Empty) { return true; }

constexpr bool x(f(Empty{}));
