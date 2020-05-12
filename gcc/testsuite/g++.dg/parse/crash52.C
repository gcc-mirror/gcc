// PR c++/39053

void foo() = // { dg-error "initialized" }
// { dg-error "-:expected" "" { target *-*-* } .+1 }
