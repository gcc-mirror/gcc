// PR c++/26071
// { dg-do compile }

struct A
{
  virtual static ~A();  // { dg-error "virtual" }
};
