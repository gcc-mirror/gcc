// PR c++/80244
// { dg-do compile { target c++11 } }

template<typename>
struct A {};

template<typename T>
using B = A<__underlying_type(T) [[gnu::aligned(4)]]>; // { dg-warning "ignoring attributes on template argument" }

template<typename T>
using B = A<__underlying_type(T) [[gnu::packed]]>; // { dg-warning "ignoring attributes on template argument" }
