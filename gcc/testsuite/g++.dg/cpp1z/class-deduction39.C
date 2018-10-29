// { dg-do compile { target c++17 } }

template <class T> struct A { };

A<int> a;
const A c = a;
volatile A v = a;
const volatile A cv = a;

template <class,class> struct same;
template <class T> struct same<T,T> {};

same<decltype(c), const A<int>> s1;
same<decltype(v), volatile A<int>> s2;
same<decltype(cv), const volatile A<int>> s3;
