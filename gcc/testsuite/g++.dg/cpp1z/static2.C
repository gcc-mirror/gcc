// { dg-additional-options -std=c++17 }

template <class T>
struct A
{
  static constexpr auto x = T::x;
};

struct B;
A<B> a;

struct B
{
  static constexpr auto x = 42;
};

auto x = a.x;
