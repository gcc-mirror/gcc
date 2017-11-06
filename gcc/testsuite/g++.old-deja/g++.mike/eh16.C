// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int e = 1;

struct A {
  ~A() {
    --e;
  }
};

struct B {
  A a;
  B() {
    throw 1;
  }
};

int
main() {
  try {
    B b;
  } catch (...) {
    return e;
  }
  return 1;
}
