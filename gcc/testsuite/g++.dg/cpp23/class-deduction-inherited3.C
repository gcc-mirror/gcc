// { dg-do compile { target c++23 } }

template<class T>
struct A {
  A(T);
  template<class U> A(T, U);
};

template<class T>
struct B : A<const T> {
  using A<const T>::A;
};

using type = decltype(B(0));
using type = decltype(B(0, 0));
using type = B<int>;
