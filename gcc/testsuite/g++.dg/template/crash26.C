// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/18471: ICE redeclaration of typedef as class template

typedef int X;			// { dg-error "previous" }
template<X> struct X {};	// { dg-error "typedef-name" }
