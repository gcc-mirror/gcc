// PR c++/21983
// { dg-do compile }

struct B { virtual void foo () = 0; };
struct D1 : public virtual B { virtual void foo () {} };
struct D2 : public virtual B { virtual void foo () {} };
struct D : public D1, public D2 { };	// { dg-warning "no unique final overrider" }
