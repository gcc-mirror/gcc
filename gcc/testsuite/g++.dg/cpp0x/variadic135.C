// { dg-options "-std=gnu++0x" }

template <typename ...> struct S;

int i = S<int,>::undefined; // { dg-error "template argument 2 is invalid" }
