// { dg-do compile { target c++11 } }

#include <type_traits>

template <typename T> struct x {
  operator bool() {
    static_assert(!std::is_same<T, T>::value, "");
    return false;
  }
};

static constexpr auto a = __is_constructible(bool, x<int>);
