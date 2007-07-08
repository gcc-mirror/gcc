// PR c++/30535

struct A {};

template<A, typename T> int operator-(A, T); // { dg-error "not a valid type" }

int i = A() - 0; // { dg-error "no match" }
