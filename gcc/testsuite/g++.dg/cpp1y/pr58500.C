// PR c++/58500
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A {};

void foo(auto (A::*)());
