// { dg-do assemble  }
// prms-id: 4677

struct A {
  A(double d) { }
};

struct B { B(A) { } } bad = 1;		// { dg-error "" } 
B good (1);
