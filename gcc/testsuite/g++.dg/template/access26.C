// PR c++/45917

template < typename T >
struct A { static int i; };
class B { typedef int X; };	// { dg-error "private" }
void f() { A<B::X>::i = 0; }	// { dg-error "this context" }
