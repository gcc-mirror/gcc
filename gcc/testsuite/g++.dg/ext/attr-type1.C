// PR c++/90750
// { dg-do compile { target c++11 } }

template <typename> struct S
{
  static const int b = 64;
};

template <typename a> struct T: S<a>
{
  using A = S<a>;
  using A::b;
  char* __attribute__((aligned(b))) c;
};

T<int> t;

#define SA(X) static_assert (X,#X)
SA (alignof(T<int>) == S<int>::b);
