// Build don't link: 
// GROUPS passed old-abort
// Special g++ Options: -Wreturn-type
#include <iostream>

class A {
public:
  friend A f(A &a);// ERROR -  ambiguates.*
};

A &f(A &a) {// ERROR -  new decl.*
  std::cout << "Blah\n";
}
