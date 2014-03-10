// PR c++/48930
// { dg-do compile { target c++11 } }
// { dg-prune-output "note" }

struct A
{
  A(const A&) = default;
};

A a;				// { dg-error "no match" }
