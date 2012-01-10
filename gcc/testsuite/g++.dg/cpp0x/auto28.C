// PR c++/51404
// { dg-options -std=c++0x }

int i = auto().x;  // { dg-error "invalid use of" }
