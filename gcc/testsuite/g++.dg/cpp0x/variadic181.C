// PR c++/99745
// { dg-do compile { target c++11 } }

template <typename... Ts>
struct S { int a : sizeof(Ts); };	// { dg-error "parameter packs not expanded with '\.\.\.':" }
S<int> s;				// { dg-message "'Ts'" "" { target *-*-* } .-1 }
template <int... Ns>
struct T { int a : Ns; };		// { dg-error "parameter packs not expanded with '\.\.\.':" }
T<0> t;					// { dg-message "'Ns'" "" { target *-*-* } .-1 }
