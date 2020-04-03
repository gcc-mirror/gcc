// PR c++/93299 - ICE in tsubst_copy with parenthesized expression.
// { dg-do compile { target c++14 } }

template <typename> struct A {
  enum { b = 8 };
};

template <int> struct __attribute__((aligned((A<int>::b)))) D { };
struct S : D<0> { };

template <int N> struct __attribute__((aligned((A<int>::b) + N))) D2 { };
struct S2 : D2<0> { };
