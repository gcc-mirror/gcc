// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int e = 1;

struct A {
  ~A() {
    --e;
  }
};

struct B : public A {
  B() {
    throw 1;
  }
};

main() {
  try {
    B b;
  } catch (...) {
    return e;
  }
  return 1;
}
