// Build don't link:
// Based on a test-case in the Standard, submitted by several people

class Outer {
  typedef int T;
  struct Inner {
    T i; // ERROR - not accessible - XFAIL *-*-*
    void f() {
      T j; // ERROR - not accessible - XFAIL *-*-*
    }
  };
};
