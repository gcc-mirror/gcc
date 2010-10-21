// PR c++/46117

struct A
{
  A (typename int); // { dg-error "before|declaration" }
};

struct B : A {};

B b;
