// PR c++/27385

struct A {};
A a[] = { 0 };  // { dg-error "initializer" }

