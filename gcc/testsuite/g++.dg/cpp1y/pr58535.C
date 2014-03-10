// PR c++/58535
// { dg-do compile { target c++1y } }
// { dg-options "" }

struct A
{
  virtual void foo(auto); // { dg-error "templates" }
};
