// Build don't link: 
// GROUPS passed constructors
#include <iostream>

class A {
   A() {}    // private constructor// ERROR - .*
};

int main() {
  A* a = new A();// ERROR - .*
  if (a) {
     std::cout << "a != NULL\n";
  } else {
     std::cout << "a == NULL\n";
  }
}


