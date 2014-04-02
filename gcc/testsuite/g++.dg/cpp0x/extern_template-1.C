// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

template <typename> void f() {}
extern template void f<int>();
