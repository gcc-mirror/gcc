// PR c++/83734
// { dg-do compile { target c++11 } }
// { dg-options "-g -O2" }

struct A { constexpr A () { typedef int T; } };
A a;
