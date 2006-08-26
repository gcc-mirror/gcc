//PR c++/28637

template<void> struct A {};  // { dg-error "not a valid type" }
A<0> a;                      // { dg-error "type" }

