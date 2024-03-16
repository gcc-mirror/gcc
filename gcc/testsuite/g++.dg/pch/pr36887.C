#include "pr36887.H"
int p1; /* { dg-error "attempt to use poisoned" } */
/* { dg-note "poisoned here" "" { target *-*-* } 1 } */
