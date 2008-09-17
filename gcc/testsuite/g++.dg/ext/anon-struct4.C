// PR c++/14401

struct { struct { int& i ; } bar ; } foo ; // { dg-error "uninitialized" "uninit" }
// { dg-warning "anonymous" "anon" { target *-*-* } 3 }
