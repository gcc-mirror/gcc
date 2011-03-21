// { dg-do compile }
// { dg-options "-femit-struct-debug-baseonly" }
struct A
{
  void foo ();
};

struct B : A
{
  typedef const A base;
  using base::foo;
};
