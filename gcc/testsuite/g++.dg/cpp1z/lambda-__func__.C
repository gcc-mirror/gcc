// PR c++/84925
// { dg-do compile { target c++17 } }

template <typename>
struct A {
  static const int value = 0;
  static auto constexpr fn = [] { return __func__; };
};

template <typename type>
int x = A<type>::value;

auto s = x<int>;
