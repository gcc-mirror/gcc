// PR c++/60626
// { dg-do compile { target c++1y } }
// { dg-options "" }

struct A {};

void (*A::p)(auto) = 0;  // { dg-error "static member|non-template" }
