// { dg-do assemble  }
// Bug: g++ silently mangles the second 'B' to 'A::B', so the definition is
// lost.

struct A {
  enum B { };
};

struct C: public A {
  enum B { };
  void foo (C::B);
};
