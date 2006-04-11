// PR c++/25856

struct A; // { dg-error "forward" } 
A::~A() {} // { dg-error "incomplete" }
