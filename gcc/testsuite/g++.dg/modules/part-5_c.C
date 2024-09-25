// { dg-additional-options -fmodules-ts }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>
import module1;

int main(int argc, char const *argv[]) {
  nmspc::Cl1 c1;
  std::cout << c1.x;
  return 0;
}
