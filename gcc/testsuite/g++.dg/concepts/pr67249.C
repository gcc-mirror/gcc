// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<class T> concept C1 = true;
template<class A, class B> struct Pair {};
// We used to test "Pair<auto, C1 >".
void f(Pair<C1 auto, C1 auto>);
