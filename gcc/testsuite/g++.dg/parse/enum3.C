// PR c++/28261

struct A {}; // { dg-message "A::A" }
// { dg-message "defined here" "" { target *-*-* } .-1 }

A::A (enum { e }) {} // { dg-error "no declaration matches" }
// { dg-error "in parameter types" "" { target *-*-* } .-1 }
