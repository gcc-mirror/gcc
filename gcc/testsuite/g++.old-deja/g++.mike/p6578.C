// { dg-do assemble  }
// prms-id: 6578

struct A {
  operator int ();
};

int i = A();
