// PR c++/51404
// { dg-options -std=c++11 }

int i = auto().x;  // { dg-error "invalid use of" }
