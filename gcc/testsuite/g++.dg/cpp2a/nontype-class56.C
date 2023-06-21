// PR c++/97700
// { dg-do compile { target c++20 } }

struct S { void (*f)(); };

template<S> struct X { };

X<S{[]{}}> x;
