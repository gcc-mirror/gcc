// PR c++/58500
// { dg-do compile { target c++1y } }
// { dg-options "" }

struct A {};

void foo(auto (A::*)());
