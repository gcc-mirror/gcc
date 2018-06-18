// PR c++/86171
// { dg-do compile { target c++11 } }

template <class> struct A;
template <class T> using B = typename A<T>::X;
template <class T> struct A {
  typedef int X;
  typedef B<T> U;
};
B<short> b;
