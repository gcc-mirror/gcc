// PR c++/48930
// { dg-options -std=c++0x }
// { dg-prune-output "note" }

struct A
{
  A(const A&) = default;
};

A a;				// { dg-error "no match" }
