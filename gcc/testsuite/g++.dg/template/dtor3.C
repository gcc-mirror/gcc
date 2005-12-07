// PR c++/19762

template<int> struct A { ~A(){} }; // { dg-error "" }
template A<>::~A(); // { dg-error "template|declaration" }
