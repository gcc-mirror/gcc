// { dg-options "-std=c++17" }

template <typename a, a> struct b;
template <typename c> struct b<bool, c::d>; // { dg-error "template parameter" }
