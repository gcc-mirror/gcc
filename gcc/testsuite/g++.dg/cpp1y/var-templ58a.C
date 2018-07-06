// PR c++/71569
// { dg-do compile { target c++14 } }

template <class T>
struct A {
  template <class U>
  static const U u;
};

template <class T>
template <class U>
const U* A<T>::u<U*> = 0;

const int *p = A<char>::u<int*>;
