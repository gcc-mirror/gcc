// PR c++/16964

struct A
{
  struct B {}; // { dg-error "previous" }
};

struct A::B{}; // { dg-error "redefinition" }
