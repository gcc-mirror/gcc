// PR c++/30535
// { dg-prune-output "note" }

struct A {};

template<A, typename T> int operator-(A, T); // { dg-error "not a valid type" }

int i = A() - 0; // { dg-error "no match" }
