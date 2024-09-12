// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }
// GROUPS passed old-abort
#include <string>

int
main(void) {

  std::string a[] = {"Hello"};

}
