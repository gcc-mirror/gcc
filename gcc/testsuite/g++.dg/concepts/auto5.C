// PR c++/101886
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename...> struct A { };

A<int, int> a;
A<auto, auto> b1 = a; // { dg-error "invalid|cannot convert" }
A<auto, auto> b2 = a; // { dg-error "invalid|cannot convert" }
