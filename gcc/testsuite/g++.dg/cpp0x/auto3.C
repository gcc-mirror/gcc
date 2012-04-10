// Negative test for auto
// { dg-do compile { target c++11 } }

#include <initializer_list>

auto x;				// { dg-error "auto" }

// If the type deduced for the template parameter U is not the same in each
// deduction, the program is ill-formed.
auto i = 42, j = 42.0;		// { dg-error "auto" }

// New CWG issue
auto a[2] = { 1, 2 };		// { dg-error "auto|initializer_list" }

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
