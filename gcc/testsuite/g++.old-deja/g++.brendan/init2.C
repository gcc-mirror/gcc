// { dg-do assemble  }
// GROUPS passed initialization
// this should give an error in require_instantiated_type about not
// being allowed to have an initializer list in an argument list.
int f(int a = {1});// { dg-error "" } .*
