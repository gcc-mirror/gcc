// PR c++/58535
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A
{
  virtual void foo(auto); // { dg-error "templates" }
};
