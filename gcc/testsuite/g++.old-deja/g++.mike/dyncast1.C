// Special g++ Options: -fexceptions -w
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe

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
  } catch (std::bad_cast) {
    return 0;
  }
  return 1;
}
