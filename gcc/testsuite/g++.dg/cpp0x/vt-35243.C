// { dg-do compile { target c++11 } }
struct A {};

template<typename... T> struct B : T...
{
  B() : T(x)... {} // { dg-error "not declared" }
};

B<A> b;
