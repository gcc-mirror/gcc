// PR c++/38698
// { dg-do compile { target c++11 } }

struct A
{
  int i;
};

A a({1,2});			// { dg-error "no match|cannot convert" }

union U
{
  int i,j;
};

U u({1,2});			// { dg-error "no match|cannot convert" }

union V {};

V v({1});			// { dg-error "no match|too many initializers" }
