// N3922
// { dg-do compile { target c++11 } }

#include <initializer_list>
template <class T, class U> struct same_type;
template <class T> struct same_type<T,T> {};

auto x1 = { 1, 2 }; // decltype(x1) is std::initializer_list<int>
same_type<decltype(x1),std::initializer_list<int>> s1;
auto x4 = { 3 }; // decltype(x4) is std::initializer_list<int>
same_type<decltype(x4),std::initializer_list<int>> s4;
auto x5{ 3 }; // decltype(x5) is int
same_type<decltype(x5),int> s5;
auto x2 = { 1, 2.0 }; // { dg-error "initializer_list" } cannot deduce element type
auto x3{ 1, 2 }; // { dg-error "one element" } not a single element
