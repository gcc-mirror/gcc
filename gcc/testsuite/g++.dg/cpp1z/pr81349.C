// PR c++/81349
// { dg-do compile { target c++17_only } }

#include <type_traits>

struct A {
  A (int) = delete;
};

struct B {
  template <typename T>
  B (T) = delete;
};

template <typename U>
struct C {
  C (U) = delete;
};

template <typename U>
struct D {
  template <typename T>
  D (T, U) = delete;
};

static_assert (std::is_aggregate_v<A>);
static_assert (std::is_aggregate_v<B>);
static_assert (std::is_aggregate_v<C<int>>);
static_assert (std::is_aggregate_v<D<int>>);
