// PR c++/51404
// { dg-do compile { target c++11 } }

int i = auto().x;  // { dg-error "9:invalid use of" }
