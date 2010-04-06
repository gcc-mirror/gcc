// { dg-options "-fshow-column" }
// PR c++/16964

struct A
{
  struct B {}; // { dg-error "10:previous definition of 'struct A::B'" }
};

struct A::B{}; // { dg-error "11:redefinition of 'struct A::B'" }
