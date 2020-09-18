// PR c++/97105
// { dg-do compile }

template <int> struct A {};
extern const A<0> a[];
const A<0> a[] = { {} };
