// { dg-do assemble  }
// { dg-options "-fexceptions" }

struct A {
  A() throw (int);
};
