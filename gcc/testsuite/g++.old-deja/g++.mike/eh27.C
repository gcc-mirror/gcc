// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

#include <typeinfo>

class MyExceptionHandler { };

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
