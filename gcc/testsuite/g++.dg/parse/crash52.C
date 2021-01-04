// PR c++/39053

void foo() = // { dg-error "initialized" }
// { dg-error "13:expected" "" { target *-*-* } .-1 }
