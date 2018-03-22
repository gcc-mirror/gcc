// PR c++/80412
// { dg-options -std=c++17 }

template <typename> struct A;
template <typename> struct B : A < B { ,  // { dg-error "" }
