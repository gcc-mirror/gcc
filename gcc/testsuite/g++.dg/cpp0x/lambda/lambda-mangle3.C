// PR c++/51818
// { dg-options -std=c++0x }
// { dg-final { scan-assembler "_ZN1AC1IN3foo3barMUlvE_EEET_" } }

struct A
{
  template <class T> A(T) { }
};

struct foo
{
  A bar = []{};
};

foo f;
