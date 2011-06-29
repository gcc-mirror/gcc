// PR c++/47635
// { dg-options -std=c++0x }

enum A { };
void A::f() { }			// { dg-error "not a class" }
