// { dg-options "-std=gnu++98" }
template <class T >
struct S
{
  S() : S() {} // { dg-message "delegating constructors" }
};

S<int> s;
