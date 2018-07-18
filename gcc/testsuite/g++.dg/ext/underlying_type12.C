// PR c++/79657
// { dg-do compile { target c++11 } }

enum A { x };
enum B { a = (__underlying_type (A)) 1 };
enum C { b = (__underlying_type (C)) 1 }; // { dg-error "incomplete" }
