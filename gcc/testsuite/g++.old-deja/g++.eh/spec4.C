// Testing exception specifications.
// Test 4: all throws fail, call terminate.

#include <stdlib.h>
#include <exception>

void my_term ()  { exit (0); }
void my_unexp () { throw 42; }

void
f () throw (short)
{
  throw 'a';
}

int main ()
{
  std::set_terminate (my_term);
  std::set_unexpected (my_unexp);

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
      return 3;
    }
  catch (std::bad_exception)
    {
      return 4;
    }
  return 5;
}
