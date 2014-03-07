// Do NSDMI get deferred instantiation?
// { dg-do compile { target c++11 } }

template <class T>
struct A
{
  T t = T(42);
  constexpr A() { }
  A(T t): t(t) { }
};

struct B { };

#define SA(X) static_assert(X,#X)

constexpr A<int> a1;
SA(a1.t == 42);

A<B> a2 {B()};
