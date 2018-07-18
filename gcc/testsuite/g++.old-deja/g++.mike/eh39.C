// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int fail = 1;
class B {
public:
  B() { throw 1; }
};
class D : public B {
public:
  D();
};

D::D() try : B() {
  fail = 1;
} catch (...) {
  fail = 0;
  throw;
}

int
main() {
  try {
    D d;
    fail = 1;
  } catch (...) {
  }
  return fail;
}
