// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/58500

struct A {};

void foo(auto (A::*)());
