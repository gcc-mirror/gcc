// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

#include <typeinfo>

class B {
public:
  int data;
  B(int i) : data(i) {
  }
} b(42);

int
main() {
  try {
    throw &b;
  } catch (B* b) {
    if (b->data == 42)
      return 0;
  }
  return 1;
}
