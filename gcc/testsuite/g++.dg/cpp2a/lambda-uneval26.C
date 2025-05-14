// PR c++/116960
// { dg-do compile { target c++20 } }

template<auto>
using Foo = decltype([](auto) { return 0; }(0));

template<typename...>
Foo<[] {}> foo() {}   // { dg-warning "no return statement" }

auto t = foo();
