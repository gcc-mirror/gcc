// { dg-do assemble  }
// { dg-options "-g" }

struct X {
  const int x[4];
};

struct A {
  A();
  A(const A&);
};

struct B {
  A a;
  int b[4];
};

struct C {
  A a;
  C() { B b=B(); };
};
