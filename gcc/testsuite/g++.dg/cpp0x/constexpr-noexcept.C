// { dg-options -std=c++11 }

template<class T>
struct is_funny {
  static constexpr bool value = false;
};

template<class T>
constexpr T value(T t) noexcept(is_funny<T>::value) { return t; } // Line 7

constexpr bool ok = noexcept(value(42));

static_assert(ok, "Assertion failure");
