// { dg-options "-std=c++17 -fconcepts" }

template <class T>
concept bool Isint = __is_same_as(T,int);

template <class T>
struct A
{
  int i;
  A(...);
};

template <Isint I>
A(I) -> A<I>;

A a(1);
A a2(1.0);			// { dg-error "" }
