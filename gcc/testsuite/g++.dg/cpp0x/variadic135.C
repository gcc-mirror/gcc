// { dg-do compile { target c++11 } }

template <typename ...> struct S;

int i = S<int,>::undefined; // { dg-error "template argument 2 is invalid" }
