// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

#include <exception>
#include <stdlib.h>

void my_terminate_handler() {
  exit(0);
}

void throw_an_unexpected_exception() throw() {
  throw 1;
}

int main() {
  std::set_terminate(my_terminate_handler);
  throw_an_unexpected_exception();
  return 1;
}
