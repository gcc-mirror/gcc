// C++29 P2287R6 - Designated-initializers for Base Classes
// { dg-do compile { target c++20 } }

struct A { int a; };
struct B : A { int b; };
void foo (A);			// { dg-message "candidate 1: 'void foo\\\(A\\\)'" "" { target c++29 } }
void foo (B);			// { dg-message "candidate 2: 'void foo\\\(B\\\)'" "" { target c++29 } }
void
bar ()
{
  foo ({ .a = 1 });		// { dg-error "call of overloaded 'foo\\\(<brace-enclosed initializer list>\\\)' is ambiguous" "" { target c++29 } }
}				// { dg-message "there are 2 candidates" "" { target c++29 } .-1 }
