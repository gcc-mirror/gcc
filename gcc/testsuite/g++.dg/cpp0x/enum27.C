// PR c++/53745
// { dg-do compile { target c++11 } }

enum E : unsigned { e = -1 };  // { dg-error "outside the range" }
