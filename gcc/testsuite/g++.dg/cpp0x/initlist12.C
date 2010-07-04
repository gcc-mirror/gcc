// PR c++/38698
// { dg-options "-std=c++0x" }
// { dg-prune-output "note" }

struct A
{
  int i;
};

A a({1,2});			// { dg-error "no match" }

union U
{
  int i,j;
};

U u({1,2});			// { dg-error "no match" }

union V {};

V v({1});			// { dg-error "no match" }
