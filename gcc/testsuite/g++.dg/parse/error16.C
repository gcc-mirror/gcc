// PR c++/16964

struct A
{
  struct B {}; // { dg-error "" }
};

struct A::B{}; // { dg-error "" }
