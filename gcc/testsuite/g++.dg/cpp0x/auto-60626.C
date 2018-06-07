// PR c++/60626
// { dg-do compile { target c++14 } }

struct A {};

void (*A::p)(auto) = 0;  // { dg-error "auto|static data member|template" }
