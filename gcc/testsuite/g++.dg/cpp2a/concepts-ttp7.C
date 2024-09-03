// PR c++/115656
// { dg-do compile { target c++20 } }

template<class T, class U> concept same_as = __is_same(T, U);

template<same_as<bool> T, template<same_as<bool>> class UU>
struct A { };

template<same_as<bool>> class B;

A<bool, B> a1;
A<long, B> a2; // { dg-error "constraint failure" }
