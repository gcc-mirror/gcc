// { dg-options "-std=c++0x" }
struct A {};

template<typename... T> struct B : T...
{
  B() : T()... {}
};

B<A> b;
