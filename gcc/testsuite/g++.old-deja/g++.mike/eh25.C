// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <exception>
#include <stdlib.h>

void my_terminate() {
  exit (0);		// Double faults should call terminate
}

struct A {
  A() { }
  ~A()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#else
  noexcept(false)
#endif
  {
    std::set_terminate (my_terminate);
    throw 1;		// This throws from EH dtor, should call my_terminate
  }
};

int
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
