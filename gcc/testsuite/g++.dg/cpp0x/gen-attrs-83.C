// { dg-do compile { target c++11 } }

struct A {};
struct B {};
struct C {};
struct D : [[]] [[]] A,
	   [[]] virtual public B, [[]] [[]] [[]] public virtual C {};
struct E : [[gnu::deprecated]] A,			// { dg-warning "attributes on base specifiers are ignored" }
	   [[gnu::deprecated]] virtual public B,	// { dg-warning "attributes on base specifiers are ignored" }
	   [[gnu::deprecated]] public virtual C {};	// { dg-warning "attributes on base specifiers are ignored" }
