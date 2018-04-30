// PR c++/70029
// { dg-lto-do assemble }

struct A
{
  A();
  int foo() && __attribute__ ((__warn_unused_result__)) { return 0; }
};

A a;
