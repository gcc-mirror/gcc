// Special g++ Options: -fexceptions -w
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

#include <typeinfo>

struct B {
  virtual int f() { }
};

struct D {
  virtual int f() { }
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
