// PR c++/123677
// { dg-do compile { target c++20 } }

struct B { void foo (); };
typedef void (B::*A) ();
struct C { constexpr C (A d) { auto e = new A (d); e->~A (); } };
C c { &B::foo };
