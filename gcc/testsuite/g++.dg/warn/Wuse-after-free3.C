// PR target/104213
// { dg-do compile }
// { dg-options "-Wuse-after-free" }
// FIXME: We should not output the warning twice.

struct A
{
  virtual ~A ();
  void f ();
};

A::~A ()
{
  operator delete (this);
  f (); // { dg-warning "used after" }
} // { dg-warning "used after" }
