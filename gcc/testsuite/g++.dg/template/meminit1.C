// { dg-options "-std=gnu++98" }
template <class T >
struct S
{
  S() : S() {} // { dg-error "base" }
};

S<int> s; // { dg-message "instantiated" }
