// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed constructors
#include <iostream>

class A {
   A() {}    // private constructor// { dg-message "" } .*
};

int main() {
  A* a = new A();// { dg-error "" } .*
  if (a) {
     std::cout << "a != NULL\n";
  } else {
     std::cout << "a == NULL\n";
  }
}


