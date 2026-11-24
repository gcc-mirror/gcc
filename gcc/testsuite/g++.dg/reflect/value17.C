// PR c++/128941
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

struct Ok
{
  static constexpr int foo() { return 3; }
  static const int value;
};
constexpr int Ok::value = Ok::foo();

struct I {
  static consteval info foo()
  { return ^^int; }

  static const info value;
};
constexpr info I::value = I::foo();

struct I2 {
  static constexpr info value = ^^int;
};
constexpr info I2::value;

template<typename T> struct J {
  static constexpr info value = ^^T;
};
template struct J<int>;
static_assert (J<int>::value == ^^int);

template<typename T> struct H {
  static constexpr info value = ^^T;
};
template const info H<int>::value;
static_assert (H<int>::value == ^^int);

template<typename T> struct K {
  static constexpr info value = ^^T;
};
static_assert (K<int>::value == ^^int);
template struct K<int>;

template<typename T> struct L {
  static constexpr info value = ^^T;
};
static_assert (L<int>::value == ^^int);
template const info L<int>::value;
