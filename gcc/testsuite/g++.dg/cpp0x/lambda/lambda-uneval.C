// 5.1.2/2: A lambda-expression shall not appear in an unevaluated operand.
// { dg-options "-std=c++0x" }

template <class T>
struct A { };
A<decltype([]{ return 1; }())> a; // { dg-error "lambda.*unevaluated context" }

