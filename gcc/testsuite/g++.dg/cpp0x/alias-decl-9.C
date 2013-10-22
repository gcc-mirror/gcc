// { dg-options "-std=c++11" }

template <class T>
struct A {
    using Result = T;
};
template <class A> using Arg = typename A::Result;
Arg<A<int>> b;

