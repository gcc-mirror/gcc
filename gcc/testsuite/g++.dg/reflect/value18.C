// PR c++/128941
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// { dg-ice "PR126811" }

using info = decltype(^^::);

struct IN {
  static consteval info foo()
  { return info{}; }

  static const info value;
};
constexpr info IN::value = IN::foo();

struct I2N {
  static constexpr info value = info{};
};
constexpr info I2N::value;

template<typename T> struct JN {
  static constexpr info value = info{};
};
template struct JN<int>;
static_assert (JN<int>::value == info{});

template<typename T> struct HN {
  static constexpr info value = info{};
};
template const info HN<int>::value;
static_assert (HN<int>::value == info{});

template<typename T> struct KN {
  static constexpr info value = info{};
};
static_assert (KN<int>::value == info{});
template struct KN<int>;

template<typename T> struct LN {
  static constexpr info value = info{};
};
static_assert (LN<int>::value == info{});
template const info LN<int>::value;
