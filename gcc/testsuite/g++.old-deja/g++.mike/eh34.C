// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

#include <exception>

void my_unexpected() {
  exit (0);
}

void foo() throw () { throw "Hi"; }

int main() {
  std::set_unexpected (my_unexpected);
  foo();
  return 1;
}
