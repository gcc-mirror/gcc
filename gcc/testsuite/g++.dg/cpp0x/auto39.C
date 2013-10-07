// PR c++/58560
// { dg-do compile { target c++11 } }

typedef auto T;     // { dg-error "typedef declared 'auto'" }

void foo() { T(); }
