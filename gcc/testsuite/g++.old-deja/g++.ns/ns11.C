// Build don't link
// Check [namespace.memdef]/2

namespace A{
  void f(int);
  void f(int,int);
  int i;              // ERROR - .*
}

void A::f(){}         // ERROR - should have been declared before

namespace B{
  void A::f(int){}    // ERROR - B does not surround A
}

int A::i;             // ERROR - redefinition

void A::f(int,int){}  // ok

