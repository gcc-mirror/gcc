// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZZN1AIiEC4IiEET_S2_Ed_NKUlvE_clEv" } }

template <class T> struct A
{
  template <class U>
  A(U, U = []{ return 42; }());
};

struct B: A<int>
{
  using A::A;
};

B b(24);
