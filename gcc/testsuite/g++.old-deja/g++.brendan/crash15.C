// Build don't link: 
// GROUPS passed old-abort
#include <iostream>

class A {
 public:
  virtual ~A() {std::cout << "executed ~A()\n";};
};

class B : public A {
 public:
  virtual ~B() {std::cout << "executed ~B()\n";};
};

int
main() {
  std::cout << "starting\n";
  B b;
  b.~A();// ERROR -  destructor
  std::cout << "done\n";
};

