// { dg-do run { xfail sparc64-*-elf z8k-*-* arm-*-pe } }
// { dg-options "-fexceptions" }

#include <exception>

void my_unexpected() {
  throw 42;
}

void foo() throw (int) { throw "Hi"; }

int main() {
  std::set_unexpected (my_unexpected);
  try {
    foo();
  } catch (int i) {
    if (i == 42)
      return 0;
  }
  return 1;
}
