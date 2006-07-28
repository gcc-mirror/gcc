//PR c++/27668

template<typename class T, T = T()> // { dg-error "nested-name-specifier|two or more|valid type" }
struct A {};                        // { dg-error "provided for" }

template<int> void foo(A<int>);     // { dg-error "wrong number" }
