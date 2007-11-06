// { dg-options "-std=c++0x" }
template<typename ...T, int = 0> struct A {}; // { dg-error "end of" }

A<int> a; // { dg-error "mismatch|expected|invalid" }
