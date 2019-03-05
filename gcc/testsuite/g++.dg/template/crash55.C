//PR c++/27668

template<typename class T, T = T()> // { dg-error "nested-name-specifier|two or more|class type|incomplete" }
// { dg-error "cast" "" { target c++98_only } .-1 }
struct A {};

template<int> void foo(A<int>);	// { dg-error "template argument 2" "" { target c++98_only } }
