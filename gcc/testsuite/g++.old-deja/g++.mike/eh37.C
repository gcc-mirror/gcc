// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

#include <typeinfo>

class B {
public:
  int data;
  B(int i) : data(i) {
  }
} b(42);

main() {
  try {
    throw &b;
  } catch (B* b) {
    if (b->data == 42)
      return 0;
  }
  return 1;
}
