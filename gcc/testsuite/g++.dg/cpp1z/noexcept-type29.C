// PR c++/126406
// { dg-do compile { target c++11 } }

template<class T> void f(T) noexcept(noexcept(T())) { }
static_assert(f<int>, "");

template<class T>
struct A {
  static void f(T) noexcept(noexcept(T())) { }
};
static_assert(A<int>::f, "");
