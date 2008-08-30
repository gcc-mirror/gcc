// Negative test for auto
// { dg-options "-std=c++0x" }

#include <initializer_list>

auto x;				// { dg-error "auto" }

// New CWG issue
auto a[2] = { 1, 2 };		// { dg-error "auto" }

template<class T>
struct A { };

A<int> A1;
// CWG issue 625
A<auto> A2 = A1;		// { dg-error "auto" }
