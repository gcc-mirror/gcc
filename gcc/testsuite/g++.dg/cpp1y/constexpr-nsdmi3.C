// PR c++/94205
// { dg-do compile { target c++14 } }

struct S
{
  struct A
  {
    S *p;
    constexpr A(S* p): p(p) {}
    constexpr operator int() { p->a = 5; return 6; }
  };
  int a = A(this);
};


constexpr S s = {};

#define SA(X) static_assert((X),#X)
SA(s.a == 6);
