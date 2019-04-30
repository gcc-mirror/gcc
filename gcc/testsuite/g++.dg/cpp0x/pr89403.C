// PR c++/89403
// { dg-do compile { target c++11 } }
// { dg-options "-Os -fsyntax-only" }

template <typename T>
struct A : T {
  constexpr A() : T() { }
};

template <typename T>
struct B {
  A<T> b;
  constexpr B() { }
};

struct C { struct {} s; };
constexpr B<C> b{};
constexpr C c = b.b;
