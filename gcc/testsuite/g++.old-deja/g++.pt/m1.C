// Build don't link: 

int f1 () {
  struct A {
    A() : b (2) { }
    int fred () { return b.hi_mom; }
    struct B {
      int hi_mom;
      B (int a) { hi_mom = a; }
    };
    B b;
  };
  A aa;
  return aa.fred();
}
/* crashes with signal 11 */
