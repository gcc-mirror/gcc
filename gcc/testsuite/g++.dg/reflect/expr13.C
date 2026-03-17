// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <type_traits>

template <auto I>
struct X {};

struct Y {
  int name;
};

template <typename S>
constexpr auto foo (S s) {
  return X<^^S::name> {};
}

constexpr auto a = foo (Y { 42 });
static_assert (std::is_same_v <decltype (a), const X <^^Y::name>>);
