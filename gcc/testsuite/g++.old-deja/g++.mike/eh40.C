// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

int fail = 1;
class B {
public:
  B() { throw 1; }
};
class D : public B {
public:
  D() try : B() {
    fail = 1;
  } catch (char c) {
    fail = 1;
    throw;
  } catch (...) {
    fail = 0;
    throw;
  }
};

main() {
  try {
    D d;
    fail = 1;
  } catch (...) {
  }
  return fail;
}
