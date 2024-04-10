// PR c++/99493
// { dg-do compile { target c++20 } }

struct owner{int m;};
struct view{const int*m;constexpr view(const owner&o):m{&o.m}{}};
template<view V>struct constant{};
template<owner O>constexpr constant<O>v{};
constexpr auto a=v<owner{}>;
