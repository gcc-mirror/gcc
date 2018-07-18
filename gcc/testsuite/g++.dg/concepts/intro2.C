// { dg-do run }
// { dg-options "-std=c++17 -fconcepts" }

#include <cassert>

template<typename T>
  concept bool C() { return __is_class(T); }

template<int N>
  concept bool P() { return true; }

C{A} struct S1
{
  P{B} int f1();
};

struct S2 {};

int main()
{
  S1<S2> s;

  assert(s.f1<10>() == sizeof(S2) + 10);
  return 0;
}

C{A} P{B} int S1<A>::f1() { return B + sizeof(A); }
