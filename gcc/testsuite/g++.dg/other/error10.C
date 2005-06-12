// PR c++/21930
// Test case by Volker Reichelt
// { dg-do compile }

template<int> struct A {};

template<int N>
void foo(const A<N> &a)
{ -A<N>(a); } // { dg-error "\\(\\(const A<0>\\*\\)a\\)" "" }

void bar()
{
    foo(A<0>()); // { dg-error "instantiated from here" "" }
}

