// PR c++/43621

template <typename T>
class A {
  template <typename>
  A<T> A<T>::f();		// { dg-error "extra qualification" }
};
