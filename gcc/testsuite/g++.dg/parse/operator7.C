// PR c++/27278
// { dg-do compile }

int operator+(void,void);  // { dg-error "incomplete type|invalid use" }
