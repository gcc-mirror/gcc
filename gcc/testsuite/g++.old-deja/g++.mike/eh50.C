// { dg-do run { xfail sparc64-*-elf z8k-*-* arm-*-pe } }
// { dg-require-effective-target c++14_down }
// { dg-options "-fexceptions" }

#include <exception>

void my_unexpected() {
  throw 42;
}

template <class T> void foo(T) throw (int) { throw "Hi"; }	// { dg-warning "deprecated" "" { target c++11 } }

int
main() {
  std::set_unexpected (my_unexpected);
  try {
    foo(1);
  } catch (int i) {
    if (i == 42)
      return 0;
  }
  return 1;
}
