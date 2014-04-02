// { dg-do compile { target c++11 } }
// Error: Explicit instantiation of a function template shall not use the
// inline or constexpr specifiers
template<class T> constexpr inline T bar(T x) { return x; }
template constexpr inline float bar(float x); // { dg-error "specifier" }
