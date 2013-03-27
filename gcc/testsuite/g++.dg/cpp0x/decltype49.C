// PR c++/45282
// { dg-require-effective-target c++11 }

struct A { int i; };
int A::*ipm = &A::i;

template <class T, class U> class assert_same_type;
template <class T> class assert_same_type<T,T> { };

assert_same_type<decltype(A().*ipm),int> x2;
