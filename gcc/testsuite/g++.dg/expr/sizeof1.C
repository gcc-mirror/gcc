// PR c++/12989

struct A
{
  int foo() { return sizeof(bar); } // { dg-error "29:ISO C\\+\\+ forbids applying .sizeof." }
  int bar();
};
