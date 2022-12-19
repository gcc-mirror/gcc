// PR c++/101886
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename...> struct A { };

A<int, int> a;
A<auto, auto> b1 = a;
A<auto, auto> b2 = a;
