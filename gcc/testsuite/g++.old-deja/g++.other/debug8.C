// Build don't link:
// Special g++ Options: -g

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
