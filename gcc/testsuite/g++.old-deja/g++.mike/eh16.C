// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf arm-*-pe

int err = 1;

struct A {
  ~A() {
    --err;
  }
};

struct B {
  A a;
  B() {
    throw 1;
  }
};

main() {
  try {
    B b;
  } catch (...) {
    return err;
  }
  return 1;
}
