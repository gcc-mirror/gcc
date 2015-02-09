// PR c++/64899
// { dg-do compile { target c++11 } }

struct S
{
  int i;
  constexpr S (): i(42) {}
};

constexpr S sa[2];
#define SA(X) static_assert((X),#X)
SA(sa[1].i == 42);
