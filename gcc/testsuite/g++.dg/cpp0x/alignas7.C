// PR c++/71513
// { dg-do compile { target c++11 } }

template < int N, typename T >
struct A
{ 
  enum alignas (N) E : T;
};

#define SA(X) static_assert((X), #X)

constexpr int al = alignof(double);
SA(alignof(A<al,char>::E) == al);
