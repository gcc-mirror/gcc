// { dg-options "-std=c++11 -pedantic" }

template <typename> void f() {}
extern template void f<int>();
