// { dg-do assemble  }
// prms-id: 9732

struct foo {};
foo& x() { return foo(); }	// { dg-warning "" } 
