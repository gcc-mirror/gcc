// PR c++/71879
// { dg-do compile { target c++11 } }

template <class T> void f(T x) { x.fail(); }
using R = decltype(&f<int>);
