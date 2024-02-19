// PR c++/113966
// { dg-do compile { target c++20 } }

template<class T> concept C = T::value;

template<class T>
struct A {
  template<class U> requires U::value || requires { { T() } -> C; }
  friend void f(A, U) { }

  template<class U> requires requires { { g(U()) } noexcept; }
  friend void f(A, U, U) { }
};

template struct A<int>;
