// P0892R2
// { dg-do compile }
// { dg-options "-std=c++2a" }

#include <type_traits>

class A {};
class B : public A {};
class C {};
class D { public: operator C() { return c; }  C c; };

template <typename T1, typename T2>
struct S {
  explicit(!std::is_convertible_v<T1, T2>)
  S(T1, T2) { }
};

void
foo ()
{
  A a;
  B b;
  C c;
  D d;

  S<int, int> s{ 1, 2 };
  S<int, int> s2 = { 1, 2 };
  S<B*, A*> s3 = { &b, &a };
  S<A*, B*> s4 = { &a, &b }; // { dg-error "converting" }
  S<B*, C*> s5 = { &b, &c }; // { dg-error "converting" }
  S<D, C> s6 = { d, c };
}
