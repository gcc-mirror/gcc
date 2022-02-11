// { dg-do run { target c++14_down } }
// Testing exception specifications.
// Test 2: the second throw succeeds.

#include <stdlib.h>
#include <exception>

void my_term ()  { exit (1); }
void my_unexp () { throw 42; }

void
f () throw (int, std::bad_exception)	// { dg-warning "deprecated" "" { target c++11 } }
{
  throw 'a';
}

int main ()
{
  std::set_terminate (my_term);
  std::set_unexpected (my_unexp); // { dg-warning "deprecated" "" { target c++11 } }

  try
    {
      f ();
    }
  catch (char)
    {
      return 2;
    }
  catch (int)
    {
      return 0;
    }
  catch (std::bad_exception)
    {
      return 4;
    }
  return 5;
}
