// { dg-do compile { target c++11 } }

struct A { };
struct B: A { };
struct C: B { using A::A; };	// { dg-error "direct" }
