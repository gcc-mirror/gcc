// PR c++/27572
// { dg-do compile }

template<typedef,int>        struct A1; // { dg-error "no type|typedef declaration" }
template<typedef x,int>      struct A2; // { dg-error "no type|typedef declaration" }
template<typedef x[],int>    struct A3; // { dg-error "no type|typedef declaration" }
template<typedef int x, int> struct A4; // { dg-error "typedef declaration" }
