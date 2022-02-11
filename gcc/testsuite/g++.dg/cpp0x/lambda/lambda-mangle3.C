// PR c++/51818
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler "_ZN1AC1IN3foo3barMUlvE_EEET_" } }
// { dg-additional-options -fno-implicit-constexpr }

struct A
{
  template <class T> A(T) { }
};

struct foo
{
  A bar = []{};
};

foo f;
