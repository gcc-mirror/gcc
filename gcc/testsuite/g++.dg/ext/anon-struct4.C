// PR c++/14401

struct { struct { int& i ; } bar ; } foo ; // { dg-error "uninitialized" "uninit" }
// { dg-warning "anonymous" "anon" { target c++98 } 3 }
// { dg-message "should be initialized" "ref-uninit" { target *-*-* } 3 }
