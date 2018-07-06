// PR c++/27572
// { dg-do compile }

template<typedef,int>        struct A1; // { dg-error "10:typedef declaration" }
// { dg-error "no type|default argument" "" { target *-*-* } .-1 }
template<typedef x,int>      struct A2; // { dg-error "10:typedef declaration" }
// { dg-error "type|default argument" "" { target *-*-* } .-1 }
template<typedef x[],int>    struct A3; // { dg-error "typedef declaration|no type|expected" }
template<typedef int x, int> struct A4; // { dg-error "10:typedef declaration" }
// { dg-error "default argument" "" { target *-*-* } .-1 }
