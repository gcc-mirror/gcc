// PR c++/30535
// { dg-prune-output "note" }

struct A {};

template<A, typename T> int operator-(A, T); // { dg-error "class type" "" { target c++17_down } }

int i = A() - 0; // { dg-error "no match" }
