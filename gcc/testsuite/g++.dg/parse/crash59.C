// PR c++/53003

struct A{ void a{} return b  // { dg-error "16:function definition" }
// { dg-error "expected" "" { target *-*-* } .-1 }
