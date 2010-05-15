// { dg-do run }
// Test that an exception thrown out of the constructor for the catch
// parameter (i.e. "after completing evaluation of the expression to be thrown
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

  try { throw A(); }
  catch (A) {}
  return 1;
}
