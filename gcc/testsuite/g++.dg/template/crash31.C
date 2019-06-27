// PR c++/19063

template<operator< struct A {}; // { dg-error "10:declaration" }
// { dg-error "expected|extra" "" { target *-*-* } .-1 }
