// { dg-additional-options "-fmodules" }

module M;

A x;
B y;

// Aliases that refer to TU-local entities should not be visible.
C c;  // { dg-error "does not name a type" }
D d;  // { dg-error "does not name a type" }
