// { dg-do compile }
// { dg-options "-Wabi -fabi-version=1" }

struct A { virtual void f(); char c1; };
struct B { B(); char c2; };
struct C : public A, public virtual B {}; // { dg-warning "ABI" }

