// Test for deduction based on transaction_safe.
// { dg-options "-fgnu-tm -std=c++11" }

void f() transaction_safe;
void g();

template <class T> struct A;
template <class R, class...Ps>
struct A<R (Ps...) transaction_safe> { };

A<decltype(f)> a;
A<decltype(g)> b;		// { dg-error "incomplete" }
