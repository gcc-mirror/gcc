// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

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
