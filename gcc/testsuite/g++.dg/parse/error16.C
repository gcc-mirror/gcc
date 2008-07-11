// { dg-options "-fshow-column" }
// PR c++/16964

struct A
{
  struct B {}; // { dg-error "12: error: previous definition of 'struct A::B'" }
};

struct A::B{}; // { dg-error "11: error: redefinition of 'struct A::B'" }
