// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

#include <cstdlib>

void myterm() {
  exit (0);
}

main() {
  try {
    throw "";
  } catch (...) {
  }
  try {
    set_terminate (myterm);
    throw;
  } catch (...) {
    return 1;
  }
  return 1;
}
