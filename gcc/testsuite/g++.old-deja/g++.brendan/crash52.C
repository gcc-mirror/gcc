// { dg-do assemble  }
// { dg-options "-Wreturn-type" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed old-abort
#include <iostream>

class A {
public:
  friend A f(A &a);// { dg-message "old declaration" }
};

A &f(A &a) {// { dg-error "new declaration" }
  std::cout << "Blah\n";
} // { dg-warning "no return statement" }
