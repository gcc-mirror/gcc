// This used to ICE (PR28420)

// { dg-do compile }

template<int> struct A;

int i = sizeof(A<typeid>); // { dg-error "operator cannot appear in a constant-expression|template argument 1 is invalid" }
