// PRMS Id: 4484 (bug 1)
// Bug: g++ does not support templates involving method pointers.
// Build don't link:

struct A {
  void f ();
};

template <class T> void
f (void (T::*p)())		// gets bogus error - use of template parm as aggregate
{ }

void g ()
{
  f (&A::f);			// gets bogus error - templates and method pointers
}
