// PR c++/58500
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

struct A {};

void foo(auto (A::*)());
