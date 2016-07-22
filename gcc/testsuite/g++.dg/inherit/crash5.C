// PR c++/71465

struct A { virtual void foo () {} };
struct B : virtual A {};
struct C : virtual A {}; 
struct D : C, B, C {};  // { dg-error "duplicate base type" }
