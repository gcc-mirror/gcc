// PR c++/68478 - flexible array members have complete type
// { dg-do compile }
// { dg-options "-Wno-error=pedantic" }

// Test to verify that attempting to use a flexible array member where
// a complete type is required is rejected.

struct A {
  int n;
  int a[];
  enum {
    e = sizeof a   // { dg-error "invalid application of .sizeof. to incomplete type" }
  };
};

struct B {
  int n;
  typedef int A[];
  A a;
  enum {
    e = sizeof a   // { dg-error "invalid application of .sizeof. to incomplete type" }
  };
};
