// PR c++/27309
// { dg-additional-options "-Wno-return-type" }

struct A
{
  int i; // { dg-message "previous" }
  A() i() {}  // { dg-error "declaration" }
};

struct B
{
  A a;
};

B b;
