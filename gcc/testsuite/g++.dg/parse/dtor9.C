// PR c++/27508
// { dg-do compile }

struct A;
using ::~A;  // { dg-error "not a class-name" }
