// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

#include <cstdlib>
#include <exception>

void myterm() {
  exit (0);
}

main() {
  try {
    throw "";
  } catch (...) {
  }
  try {
    std::set_terminate (myterm);
    throw;
  } catch (...) {
    return 1;
  }
  return 1;
}
