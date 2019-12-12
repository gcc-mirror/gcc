// { dg-do assemble  }
// GROUPS passed error-messages
void foo (mutable int x);// { dg-error "11:non-member .x. cannot be declared .mutable." }  non-member `x' cannot be declared `mutable'.*
