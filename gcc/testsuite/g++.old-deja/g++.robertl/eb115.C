// { dg-do run  }
// { dg-options "-O" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>
#include <typeinfo>

int main() {
  int *i1, *i2;
  std::cerr << (typeid(i1)==typeid(i2)) << std::endl;
}
