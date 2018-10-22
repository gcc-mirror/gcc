// PR c++/66218
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

#include <type_traits>

template <class T, class U>
concept bool Same =
  std::is_same<T, U>::value;

template <class T>
concept bool C =
  requires(T t) {
    { t } -> Same<T>;
  };

template <class>
constexpr bool f() { return false; }
template <C>
constexpr bool f() { return true; }

static_assert(f<char>(), "");
static_assert(f<int>(), "");
static_assert(f<double>(), "");

int main() {}
