// PR c++/47635
// { dg-options -std=c++11 }

enum A { };
void A::f() { }			// { dg-error "not a class" }
