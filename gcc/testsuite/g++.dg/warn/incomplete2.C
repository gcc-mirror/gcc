// PR c++/33501
// { dg-do compile }

class A;	// { dg-error "forward declaration" }

int f (A);
const A &make ();

int
main ()
{
  return f (make ());	// { dg-error "invalid use of incomplete type|initializing argument" }
}
