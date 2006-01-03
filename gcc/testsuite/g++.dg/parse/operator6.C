// PR c++/25635

struct A {};

A::operator int(); // { dg-error "class" }
