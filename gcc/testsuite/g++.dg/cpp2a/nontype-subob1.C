// { dg-do compile { target c++20 } }

template <auto N> struct A {};
template <class,class> struct assert_same;
template <class T> struct assert_same<T,T> {};

#define TEQ(X,Y) static_assert(__is_same(A<(X)>,A<(Y)>))
#define TNEQ(X,Y) static_assert(!__is_same(A<(X)>,A<(Y)>))

struct C { int i; };

struct B: C
{
  int j[3];
} b;

// { dg-final { scan-assembler _Z1f1AIXaddtL_Z1bE1iEE } }
void f(A<&b.i>) {}
TEQ(&b.i,&((C*)&b)->i);

// { dg-final { scan-assembler "_Z1g1AIXadixdtL_Z1bE1jL\[silx]1EEE" } }
void g(A<&b.j[0]+1>) {}
TEQ(&b.j[1],&b.j[1]);
TEQ(&b.j[1],&b.j[0]+1);
TNEQ(&b.j[1],&b.j[0]);
