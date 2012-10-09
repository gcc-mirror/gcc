// PR c++/54323
// { dg-do compile { target c++11 } }

template<bool, typename T = void>
struct enable_if { };

template<typename T>
struct enable_if<true, T>
{ typedef T type; };

template<template<typename> class CRTP, typename T>
class Base
{
public:
  template<template<typename> class CRTP0, typename T0, class>
  friend int func(const Base<CRTP0, T0>& rhs);

protected:
  int n;
};

template<template<typename> class CRTP0, typename T0,
	 class = typename enable_if<true>::type>
int func(const Base<CRTP0, T0>& rhs)
{
  return rhs.n;
}

template<typename T>
class Derived : public Base<Derived, T> {};

int main()
{
  Derived<int> x;
  func(x);
  return 0;
}
