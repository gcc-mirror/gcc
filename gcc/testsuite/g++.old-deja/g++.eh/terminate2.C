// { dg-do run  }
// Test that an unhandled exception causes us to call terminate.

#include <exception>
#include <cstdlib>

void my_terminate ()
{
  std::exit (0);
}

int main (void)
{
  std::set_terminate (my_terminate);
  throw 1;
  return 1;
}
