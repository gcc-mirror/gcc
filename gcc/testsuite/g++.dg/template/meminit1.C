template <class T >
struct S
{
  S() : S() {} // { dg-error "base" }
};

S<int> s; // { dg-error "instantiated" }
