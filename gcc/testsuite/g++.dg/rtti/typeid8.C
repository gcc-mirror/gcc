// PR c++/36405
// { dg-do compile }

#include <typeinfo>

struct A
{
  void foo ()
  {
    typeid (foo).name ();	// { dg-error "invalid use of member" }
    typeid (A::foo).name ();	// { dg-error "invalid use of member" }
  }
  void bar ()
  {
    typeid (foo).name ();	// { dg-error "invalid use of member" }
    typeid (A::foo).name ();	// { dg-error "invalid use of member" }
  }
  static void baz ()
  {
    typeid (baz).name ();
    typeid (A::baz).name ();
  }
};

const char *p1 = typeid (A::foo).name ();	// { dg-error "invalid use of non-static member" }
const char *p2 = typeid (A::baz).name ();
