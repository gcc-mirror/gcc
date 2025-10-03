// PR c++/122112
// { dg-do compile { target c++20 } }

void func(struct { auto x; });  // { dg-error "" }
