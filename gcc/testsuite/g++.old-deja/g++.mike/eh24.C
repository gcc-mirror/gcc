// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*

int fail = 0;

struct A {
  int ok;
  A() {
    ok = 1;
  }
  ~A() {
    if (! ok)
      fail = 1;
    ok = 0;
  }
};

main() {
  try {
    try {
      A  a;
      throw 1.0;
    } catch (double i) {
      A a1;
      throw 1;    // make sure both a1 and a2 are not destroyed when we throw!
    } catch (int i) {
      A a2;
      throw 1.0;
    }
  } catch (int i) {
  }
  return fail;
}
