// PR c++/59838
// { dg-do compile }

enum E { a, b = (E) a }; // { dg-error "conversion to incomplete type" }
