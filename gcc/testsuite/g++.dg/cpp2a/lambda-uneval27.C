// PR c++/117530
// { dg-do compile { target c++20 } }

template <class> struct A;
template <class T> using B = decltype([]() -> A<T>::X { return 0; });
template <class T> struct A {
  typedef int X;
  typedef B<T> U;
};
B<short> b;
