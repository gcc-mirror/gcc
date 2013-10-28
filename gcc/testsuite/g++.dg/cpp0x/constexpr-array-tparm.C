// { dg-options -std=c++11 }

template <const int I[2]> struct A { int ir[I[0]]; };
extern constexpr int ar[2] = { 1, 2 };
A<ar> a;
