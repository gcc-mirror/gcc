// { dg-do assemble  }
// PRMS Id: 4484 (bug 1)
// Bug: g++ does not support templates involving method pointers.

struct A {
  void f ();
};

template <class T> void
f (void (T::*p)())		// { dg-bogus "" } use of template parm as aggregate
{ }

void g ()
{
  f (&A::f);			// { dg-bogus "" } templates and method pointers
}
