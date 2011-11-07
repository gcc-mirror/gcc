// { dg-options "-std=c++0x" }

template <class T>
struct A {
    using Result = T;
};
template <class A> using Arg = typename A::Result;
Arg<A<int>> b;

