// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

#include <exception>
#include <stdlib.h>

void my_unexpected() {
  exit (0);
}

void foo() throw () { throw "Hi"; }

int main() {
  std::set_unexpected (my_unexpected);
  foo();
  return 1;
}
