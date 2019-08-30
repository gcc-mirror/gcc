// PR c++/87148
// { dg-do compile }
// { dg-options "-pedantic" }

struct Tst { int i; char t[]; };	// { dg-warning "forbids flexible array member" }

Tst t {};				// { dg-warning "extended initializer lists only available with" "" { target c++98_only } }
Tst u = Tst();
void foo () { Tst u = {}; }
Tst *bar () { return new Tst (); }
