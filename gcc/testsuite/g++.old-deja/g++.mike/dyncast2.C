// Special g++ Options: -fexceptions -w
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

// Ensure reference handling works.

#include <typeinfo>

struct B {
  virtual int f() { }
} ob;

struct D : public B {
  virtual int f() { }
} od;

main() {
  B *b=&ob;
  try {
    void *vp = &dynamic_cast<D&>(*b);
    return 1;
  } catch (std::bad_cast) {
    return 0;
  }
  return 1;
}
