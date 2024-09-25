// PR c++/105512
// { dg-additional-options -fmodules-ts }
// { dg-module-do run }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>
import Hello2;

int main() {
  std::cout << tester();
}
