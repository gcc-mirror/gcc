// { dg-do assemble  }
// Based on a testcase in the Standard, submitted by several people

class Outer {
  typedef int T;
  struct Inner {
    T i; // { dg-error "" "" { xfail *-*-* } } not accessible - 
    void f() {
      T j; // { dg-error "" "" { xfail *-*-* } } not accessible - 
    }
  };
};
