// PR c++/58882
// { dg-do compile }
// { dg-options "" }

int a[] = { [0.] = 0 }; // { dg-error "integral constant-expression" }
