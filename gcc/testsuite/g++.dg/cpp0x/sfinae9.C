// PR c++/48450
// { dg-do compile { target c++11 } }

namespace std {
  template <class T> T&& declval();
}

template<class To, class From,
  class = decltype(static_cast<To>(std::declval<From>()))
>
char f(int);

template<class, class>
char (&f(...))[2];

struct A { virtual ~A() = 0; };
struct B {};
struct D : B {};

static_assert(sizeof(f<A, int>(0)) != 1, "Error"); // a
static_assert(sizeof(f<int*, const void*>(0)) != 1, "Error"); // b
static_assert(sizeof(f<D*, const B*>(0)) != 1, "Error"); // c
static_assert(sizeof(f<int B::*, const int D::*>(0)) != 1, "Error"); // d
static_assert(sizeof(f<B, void>(0)) != 1, "Error"); // e
