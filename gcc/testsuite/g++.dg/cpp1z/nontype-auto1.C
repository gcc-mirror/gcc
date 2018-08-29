// Testcase from P0127R2
// { dg-options -std=c++17 }

template <long n> struct A { };

template <class T> struct C;
template <class T, T n> struct C<A<n>>
{
    using Q = T;
};

typedef long R;
typedef C<A<2>>::Q R;  // OK; T was deduced to long from the template argument value in the type A<2>
