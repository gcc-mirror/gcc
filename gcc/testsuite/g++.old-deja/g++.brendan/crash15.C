// Build don't link: 
// GROUPS passed old-abort
#include <iostream.h>

class A {
 public:
  virtual ~A() {cout << "executed ~A()\n";};
};

class B : public A {
 public:
  virtual ~B() {cout << "executed ~B()\n";};
};

int
main() {
  cout << "starting\n";
  B b;
  b.~A();// ERROR -  destructor
  cout << "done\n";
};

