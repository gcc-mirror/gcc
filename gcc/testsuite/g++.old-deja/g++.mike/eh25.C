// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <exception>
#include <stdlib.h>

void my_terminate() {
  exit (0);		// Double faults should call terminate
}

struct A {
  A() { }
  ~A() {
    std::set_terminate (my_terminate);
    throw 1;		// This throws from EH dtor, should call my_terminate
  }
};

main() {
  try {
    try {
      throw 1;
    } catch (int i) {
      A a;		// A hit on this EH dtor went to the wrong place
      throw 1;
    }
  } catch (...) {
    return 1;
  }
  return 1;
}
