// Caught by Booch Components.
// Bug: g++ tries to instantiate nested enums.
// Build don't link:

template <class T> struct A
{
  struct B { };
  enum C { c };
};

template struct A<int>;
