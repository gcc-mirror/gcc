// { dg-do assemble  }
// Test to make sure that the array bounds are treated as an expression
// in template parm substitution.

// Written by Jason Merrill <jason@cygnus.com>


template <class T> void f (T t) { int ar[sizeof (T)]; }

int
main ()
{
  f (1);
}
