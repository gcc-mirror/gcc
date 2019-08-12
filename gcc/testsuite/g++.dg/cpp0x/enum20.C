// PR c++/47635
// { dg-do compile { target c++11 } }

enum A { };
void A::f() { }			// { dg-error "6:.enum A. is not a class" }
