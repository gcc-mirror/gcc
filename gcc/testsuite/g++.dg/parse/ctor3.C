// PR c++/25633

struct A {};

struct B : A
{
  B() : A {} // { dg-error "expected" }
};
