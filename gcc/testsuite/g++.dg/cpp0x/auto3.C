// Negative test for auto
// { dg-options "-std=c++0x" }

#include <initializer_list>

auto x;				// { dg-error "auto" }

// If the type deduced for the template parameter U is not the same in each
// deduction, the program is ill-formed.
auto i = 42, j = 42.0;		// { dg-error "" "" { xfail *-*-* } }

// New CWG issue
auto a[2] = { 1, 2 };		// { dg-error "auto" }

template<class T>
struct A { };

A<int> A1;
// CWG issue 625
A<auto> A2 = A1;		// { dg-error "" }

auto foo() { }			// { dg-error "auto" }

void bar(auto i)		// { dg-error "incomplete|auto" }
{
  (void)i;
}
