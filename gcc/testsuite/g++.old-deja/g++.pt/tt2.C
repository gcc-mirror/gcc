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

int f2 () {
  struct A {
    ~A() { a = 3; }
    int a;
    int fred () { return a + 1; }
  };

  A ab;
  ab.a = 12;
  return ab.fred();
}
