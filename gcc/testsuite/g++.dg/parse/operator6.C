// PR c++/25635

struct A {}; // { dg-message "defined here" }

A::operator int(); // { dg-error "no declaration matches" }
// { dg-message "no conversion operators" "" { target *-*-* } .-1 }
