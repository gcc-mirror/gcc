// PR c++/52759
// { dg-do compile }
// { dg-require-weak "" }
// { dg-options "" }
#pragma weak foo
template <typename T>
struct A { };
template <typename T>
void bar (A<T> &);
