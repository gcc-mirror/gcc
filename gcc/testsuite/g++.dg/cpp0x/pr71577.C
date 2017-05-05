// PR c++/71577
// { dg-do compile { target c++11 } }

struct { int a; } s1, s2 = { s1, 0 };  // { dg-error "too many initializers" }
