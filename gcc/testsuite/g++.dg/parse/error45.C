// PR c++/51429
// { dg-do compile }

struct A
{
  void foo (double);
  void foo (int);
  A () { foo = 0; }	// { dg-error "invalid use of member function" }
};
