// Special g++ Options: -fexceptions -w
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe powerpc-*-eabi

#include <typeinfo>

struct B {
  virtual f() { }
};

struct D {
  virtual f() { }
};

main() {
  B b;
  try {
    (void)dynamic_cast<D&>(b);
  } catch (bad_cast) {
    return 0;
  }
  return 1;
}
