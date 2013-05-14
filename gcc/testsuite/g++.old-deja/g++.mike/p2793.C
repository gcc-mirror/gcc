// { dg-do assemble  }
// prms-id: 2793

void f(char&) {			// { dg-message "" } referenced by error below
  f('c');			// { dg-error "" } 
}
