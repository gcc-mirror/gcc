// PR c++/19762

template<int> struct A { ~A(){} }; // { dg-message "provided for" }
template A<>::~A(); // { dg-error "template|declaration" }
