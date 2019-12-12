// { dg-do compile { target c++11 } }

template<class T>
struct is_funny {
  static constexpr bool value = false;
};

template<class T>
constexpr T value(T t) noexcept(is_funny<T>::value) { return t; } // Line 7

constexpr bool ok = noexcept(value(42));

// We used to treat a call to a constexpr function as noexcept if
// the call was a constant expression.  We no longer do since
// c++/87603.
static_assert(!ok, "Assertion failure");
