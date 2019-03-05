// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<class T> concept bool C1 = true;
template<class A, class B> struct Pair {};
void f(Pair<auto, C1>);
