// Build don't link:
// prms-id: 4677

struct A {
  A(double d) { }
};

struct B { B(A) { } } bad = 1;		// ERROR - 
B good (1);
