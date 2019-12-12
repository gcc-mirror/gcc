// PR c++/52707
// { dg-do compile { target c++11 } }

struct A {
 int m;
 A() = delete;
};

A a = {1};	// { dg-error "could not convert" "" { target c++2a } }
