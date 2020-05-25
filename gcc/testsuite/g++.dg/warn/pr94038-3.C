// PR c++/94038
// { dg-do compile { target c++20 } }
// { dg-additional-options "-Wall" }

template<typename T>
constexpr int foo() {
  return T::x;
}

constexpr bool bar(bool a) { return a; }

template<typename T>
concept C = foo<T>() == 0;

static_assert(decltype(bar(C<int>)){} == false);
