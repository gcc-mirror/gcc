// { dg-do assemble  }
// Caught by Booch Components.
// Bug: g++ tries to instantiate nested enums.

template <class T> struct A
{
  struct B { };
  enum C { c };
};

template struct A<int>;
