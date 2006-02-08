// PR c++/26070
// { dg-do compile }

struct A
{
  virtual static int i;  // { dg-error "virtual" }
};
