// PR c++/64314
// { dg-do compile { target c++11 } }

struct C { C(); ~C(); };
struct A {
  int i;
  C c[1];
} a {};
