//PR c++/28640

template<void> struct A; // { dg-error "not a valid type" }
template<int> struct A;
 
