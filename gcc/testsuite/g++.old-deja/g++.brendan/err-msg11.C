// { dg-do assemble  }
// GROUPS passed error-messages
void foo (mutable int x);// { dg-error "" }  non-member `x' cannot be declared `mutable'.*
