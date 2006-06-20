// PR c++/28052
// { dg-do compile }

struct A
{
  double d : 2;  // { dg-error "non-integral" }
  A() {}
  ~A() {}
};
