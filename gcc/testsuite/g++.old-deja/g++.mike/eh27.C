// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <typeinfo>

class MyExceptionHandler { };

int
main() {
  try {
    throw MyExceptionHandler();
  } catch(const MyExceptionHandler& eh) {
    return 0;
  } catch(...) {
    return 1;
  }
  return 1;
}
