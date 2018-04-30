// PR c++/27211

struct A {}; // { dg-message "defined here" }

template<int> void A::foo() {} // { dg-error "no declaration matches" }
// { dg-message "no functions named" "note" { target *-*-* } .-1 }
