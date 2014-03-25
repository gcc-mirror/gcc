// 5.1.2/2: A lambda-expression shall not appear in an unevaluated operand.
// { dg-do compile { target c++11 } }

template <class T>
struct A { };
A<decltype([]{ return 1; }())> a; // { dg-error "lambda.*unevaluated context" }

// { dg-prune-output "template argument" }
// { dg-prune-output "invalid type" }
