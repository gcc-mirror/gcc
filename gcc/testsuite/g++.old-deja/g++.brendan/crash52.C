// Build don't link: 
// GROUPS passed old-abort
#include <iostream.h>

class A {
public:
  friend A f(A &a);// ERROR -  ambiguates.*
};

A &f(A &a) {// ERROR -  new decl.*
  cout << "Blah\n";
} // ERROR - non-void function

