// PR c++/107542
// { dg-do compile { target c++20 } }

#include <compare>

template<class T, class U>
concept same_as = __is_same(T, U);

template<class Lhs, class Rhs>
concept Ord = requires(Lhs lhs, Rhs rhs) {
  { lhs <=> rhs } -> same_as<std::strong_ordering>;
};

static_assert(Ord<int*, int*>);   // Works.
static_assert(!Ord<int*, char*>); // ICE.

template<class T>
struct S {
  T* p;
};

template<class T, class U>
  requires(Ord<const T*, const U*>)
constexpr inline auto operator<=>(const S<T>& l, const S<U>& r) noexcept {
  return l.p <=> r.p;
}

static_assert(Ord<S<int>, S<int>>);   // Works.
static_assert(!Ord<S<int>, S<char>>); // ICE.
