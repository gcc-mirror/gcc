// { dg-do run  }
// Test that an exception thrown out of the constructor for the exception
// object (i.e. "after completing evaluation of the expression to be thrown
// but before the exception is caught") causes us to call terminate.

#include <exception>
#include <cstdlib>

void my_terminate ()
{
  std::exit (0);
}

struct A
{
  A () {}
  A (const A&) { throw 1; }
};

int main (void)
{
  std::set_terminate (my_terminate);

  A a;
  try { throw a; }
  catch (...) {}
  return 1;
}
