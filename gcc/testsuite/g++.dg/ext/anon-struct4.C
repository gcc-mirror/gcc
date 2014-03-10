// PR c++/14401

struct { struct { int& i ; } bar ; } foo ; // { dg-error "deleted|uninitialized" "uninit" }
// { dg-warning "anonymous" "anon" { target { ! c++11 } } 3 }
// { dg-message "should be initialized" "ref-uninit" { target { ! c++11 } } 3 }
