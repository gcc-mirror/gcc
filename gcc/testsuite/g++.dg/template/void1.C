// PR c++/27430
// { dg-do compile }

template<void[]> struct A;  // { dg-error "array of void" }
