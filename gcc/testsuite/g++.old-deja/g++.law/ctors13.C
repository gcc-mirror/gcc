// Build don't link: 
// GROUPS passed constructors
#include <iostream.h>

class A {
   A() {}    // private constructor// ERROR - .*
};

int main() {
  A* a = new A();// ERROR - .*
  if (a) {
     cout << "a != NULL\n";
  } else {
     cout << "a == NULL\n";
  }
}
