// PR c++/85264
// { dg-do compile { target c++11 } }

template<typename> struct A {};

template<int>
template<typename... T>
struct A<void(T...)> {};    // { dg-error "too many template-parameter-lists" }

A<void(int)> a;
