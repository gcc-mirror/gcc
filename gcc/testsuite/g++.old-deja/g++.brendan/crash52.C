// { dg-do assemble  }
// { dg-options "-Wreturn-type" }
// GROUPS passed old-abort
#include <iostream>

class A {
public:
  friend A f(A &a);// { dg-error "" }  ambiguates.*
};

A &f(A &a) {// { dg-error "" }  new decl.*
  std::cout << "Blah\n";
}
