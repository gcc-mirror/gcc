// { dg-do assemble  }
// Bug: g++ decides that A::foo is introducing a constructor declarator.

struct A {
  typedef bool foo;
};

A::foo (*bar) ();

struct B {
  A::foo (*bar) ();
};
