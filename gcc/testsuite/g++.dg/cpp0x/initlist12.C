// PR c++/38698
// { dg-options "-std=c++0x" }

struct A
{
  int i;
};

A a({1,2});			// { dg-error "too many initializers" }

union U
{
  int i,j;
};

U u({1,2});			// { dg-error "too many initializers" }

union V {};

V v({1});			// { dg-error "too many initializers" }
