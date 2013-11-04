// PR c++/58979
// { dg-do compile }

int i = 0->*0; // { dg-error "invalid type argument of" }
