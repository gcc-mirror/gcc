// { dg-do assemble  }
// Build don't link
// Check [namespace.memdef]/2

namespace A{
  void f(int);
  void f(int,int);
  int i;              // { dg-error "" } .*
}

void A::f(){}         // { dg-error "" } should have been declared before

namespace B{
  void A::f(int){}    // { dg-error "" } B does not surround A
}

int A::i;             // { dg-error "" } redefinition

void A::f(int,int){}  // ok

