// PR c++/68508
// { dg-do compile }
// { dg-options "-std=c++14 -fsanitize=vptr" }

struct A
{
  virtual int foo () { return 0; }
};

const A &
bar ()
{
  static A x = A ();
  return (x);
}
