// { dg-do run { xfail sparc64-*-elf arm-*-pe } }
// { dg-options "-fexceptions" }

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

int
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
