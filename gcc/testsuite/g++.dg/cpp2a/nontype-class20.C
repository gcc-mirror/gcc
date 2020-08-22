// PR c++/90098
// { dg-do compile { target c++20 } }

struct A {
  int value;
  // auto operator<=>(const A&) = default;
};

template<A... Us>
struct Z {};

template<A V, A... Rest>
struct Z<V, Rest...> {};
