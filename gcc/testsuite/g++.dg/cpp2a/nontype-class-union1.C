// { dg-do compile { target c++20 } }

template <auto N> struct A {};
template <class,class> struct assert_same;
template <class T> struct assert_same<T,T> {};

#define TEQ(X,Y) static_assert(__is_same(A<(X)>,A<(Y)>))
#define TNEQ(X,Y) static_assert(!__is_same(A<(X)>,A<(Y)>))

union U {
  int i; int j;
  constexpr U(int i): i(i) {}
  constexpr U(unsigned u): j(u) {}
};

TEQ(U(0),U(0));

// Calling the other constructor initializes a different member with the same
// value.  We need to distinguish these.
TNEQ(U(0),U(0u));

// { dg-final { scan-assembler "_Z1f1AIXtl1UEEE" } }
void f(A<U(0)>) { }
// { dg-final { scan-assembler "_Z1g1AIXtl1Udi1jLi0EEEE" } }
void g(A<U(0u)>) { }
