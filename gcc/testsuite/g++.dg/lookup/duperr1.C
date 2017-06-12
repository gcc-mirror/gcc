// PR c++/29048

class A { int i; }; // { dg-bogus "is private.*is private" }
// { dg-message "private" "" { target *-*-* } .-1 }
class B:public A { B() { A::i=0; } }; // { dg-error "within this context" }
