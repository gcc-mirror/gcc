// { dg-do assemble  }
// prms-id: 2793

void f(char&) {			// { dg-error "" } referenced by error below
  f('c');			// { dg-error "" } 
}
