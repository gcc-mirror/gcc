// { dg-do assemble  }
// Submitted by Jason Merrill <jason@cygnus.com>.
// Bug: g++ fails to see through the T typedef in the C ctor.

struct A {
  A (int) { }
};

typedef A T;

struct B: public virtual T {
  B (): T(1) { }
};

struct C: public B {
  C (): T(1) { }
};
