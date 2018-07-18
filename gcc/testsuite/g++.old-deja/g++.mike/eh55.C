// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <exception>
#include <stdlib.h>

void my_terminate_handler() {
  exit(0);
}

void throw_an_unexpected_exception() throw() {
  throw 1;	// { dg-warning "throw will always call terminate" "" { target c++17 } }
}

int main() {
  std::set_terminate(my_terminate_handler);
  throw_an_unexpected_exception();
  return 1;
}
