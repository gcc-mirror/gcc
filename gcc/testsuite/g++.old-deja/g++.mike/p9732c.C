// { dg-do assemble  }
// prms-id: 9732

struct foo {};
foo& x() { return foo(); }	// { dg-error "cannot bind non-const lvalue" } 
