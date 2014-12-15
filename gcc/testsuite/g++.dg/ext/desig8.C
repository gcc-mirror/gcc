// PR c++/58882

int a[] = { [0.] = 0 }; // { dg-error "integral constant-expression" }
