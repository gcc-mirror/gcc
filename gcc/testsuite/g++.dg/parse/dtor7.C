// PR c++/25856

struct A; // { dg-message "forward" } 
A::~A() {} // { dg-error "incomplete" }
