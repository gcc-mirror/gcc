// PR c++/12989

struct A
{
  int foo() { return sizeof(bar); } // { dg-error "" }
  int bar();
};
