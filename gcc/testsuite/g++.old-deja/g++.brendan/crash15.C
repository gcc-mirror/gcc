// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed old-abort
#include <iostream>

class A {
 public:
  virtual ~A() {std::cout << "executed ~A()\n";}
};

class B : public A {
 public:
  virtual ~B() {std::cout << "executed ~B()\n";}
};

int
main() {
  std::cout << "starting\n";
  B b;
  b.~A();
  std::cout << "done\n";
}

