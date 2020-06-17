// PR c++/25633

struct A {};

struct B : A
{
  B() : A {} // { dg-error "initializer" "" { target c++98_only } }
};			// { dg-error "expected" }
