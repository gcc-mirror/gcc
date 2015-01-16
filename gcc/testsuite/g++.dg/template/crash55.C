//PR c++/27668

template<typename class T, T = T()> // { dg-error "nested-name-specifier|two or more|valid type" }
struct A {};

template<int> void foo(A<int>);	// { dg-error "cast|argument" "" { target c++98_only } }
