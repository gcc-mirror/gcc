// Identified when investigating PR c++/109918: no warning was emitted due to
// an incorrect early return in warn_hidden.
// { dg-additional-options -Wall }

struct Foo
{
  virtual void f(int); // { dg-warning "hidden" }
  virtual void g() {}
};

struct Bar : Foo
{
  virtual void f(short); // { dg-message "by" }
  virtual void g() {}
};
