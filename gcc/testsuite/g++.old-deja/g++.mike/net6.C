// { dg-do assemble  }
// { dg-options "" }

struct X {};
X x = X();		// { dg-bogus "" } 
