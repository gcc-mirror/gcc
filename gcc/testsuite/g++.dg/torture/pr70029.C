// PR c++/70029
// { dg-do compile }
// { dg-options "-std=c++11 -g -flto" }
// { dg-require-effective-target lto }

struct A
{
  A();
  int foo() && __attribute__ ((__warn_unused_result__)) { return 0; }
};

A a;
