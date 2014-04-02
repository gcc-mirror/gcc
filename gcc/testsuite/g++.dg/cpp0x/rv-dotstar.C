// PR c++/49389
// { dg-do compile { target c++11 } }

template<class T> T&& val();

struct A {};

typedef decltype(val<A>().*val<int A::*>()) type;

template<class> struct assert_type;
template<> struct assert_type<int&&> {};

assert_type<type> test;
