// { dg-do compile }

struct A {};

struct B
{
  friend A::~B();  // { dg-error "as member of" }
};
