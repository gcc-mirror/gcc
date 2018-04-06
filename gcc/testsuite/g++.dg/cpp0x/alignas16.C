// PR c++/85062
// { dg-do compile { target c++11 } }

template<typename... T> struct A
{
  int alignas(T...) i;		// { dg-warning "ignored" }
};

A<int> a;
