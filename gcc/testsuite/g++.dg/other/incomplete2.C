// PR c++/19963
// { dg-do compile }

struct A;

struct B
{
  A a : 1;  // { dg-error "incomplete" }
};

struct S
{
  S : 1;    // { dg-error "incomplete" }
};
