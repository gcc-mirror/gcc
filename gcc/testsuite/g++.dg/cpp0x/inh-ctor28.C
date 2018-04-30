// PR c++/81164
// { dg-do compile { target c++11 } }

struct A {};
struct B : virtual A {};
struct C : virtual A {};
struct D : B,C { using A::A; };	// { dg-error "indirect" }
