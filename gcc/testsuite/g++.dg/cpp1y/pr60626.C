// PR c++/60626
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A {};

void (*A::p)(auto) = 0;  // { dg-error "static data member|template" }
