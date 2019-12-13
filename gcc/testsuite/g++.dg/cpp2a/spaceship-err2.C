// { dg-do compile { target c++2a } }

#include <compare>
template <class T, T x = (T() <=> T())> // { dg-error "31:0 <=> 0" }
void f(T);
//constexpr int f(...) { return 42; }
constexpr int i = f(24);	//  { dg-error "no match" }
