// { dg-do assemble  }
// { dg-options "" }

int *foo = new int[1](0); // { dg-bogus "" } 
