// { dg-do run  }
// Testcase for proper handling of rethrow.

#include <stdio.h>

int c, d;
int wrong;

struct A
{
  int i;
  A () { i = c++; printf ("A() %d\n", i); }
  A (const A&) { i = c++; printf ("A(const A&) %d\n", i); }
  ~A() { printf ("~A() %d\n", i); ++d; }
};

struct B
{
  ~B () {
    try
      {
	printf ("Rethrowing III...\n");
	throw;
      }
    catch (A& a)
      {
	printf ("Caught III %d...\n", a.i);
	if (a.i != 1)
	  {
	    printf ("** rethrew uncaught exception **\n");
	    wrong = 1;
	  }
      }
    printf ("continuing to unwind II...\n");
  }
};

int
main ()
{
  {
    A a;

    try
      {
	try
	  {
	    printf ("Throwing I...\n");
	    throw a;
	  }
	catch (A& a)
	  {
	    printf ("Caught I %d...\n", a.i);
	    try
	      {
		B b;
		printf ("Throwing II...\n");
		throw a;
	      }
	    catch (A& a)
	      {
		printf ("Caught II %d...\n", a.i);
		printf ("Throwing IV...\n");
		throw;
	      }
	  }
      }
    catch (A& a)
      {
	printf ("Caught IV %d.\n", a.i);
      }
  }

  printf ("c == %d, d == %d\n", c, d);
  return c != d || wrong;
}
