// PR c++/49389
// { dg-options -std=c++0x }

template<class T> T&& val();

struct A {};

typedef decltype(val<A>().*val<int A::*>()) type;

template<class> struct assert_type;
template<> struct assert_type<int&&> {};

assert_type<type> test;
