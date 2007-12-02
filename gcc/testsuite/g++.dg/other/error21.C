// PR c++/34273

struct A {};

struct B : A
{
  B() : A()... {} // { dg-error "cannot expand" }
};
