// { dg-options "-std=c++0x -pedantic" }

template <typename> void f() {}
extern template void f<int>();
