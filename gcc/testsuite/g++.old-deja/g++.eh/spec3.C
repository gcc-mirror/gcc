// Testing exception specifications.
// Test 3: the bad_exception throw succeeds.

#include <stdlib.h>
#include <exception>

void my_term ()  { exit (1); }
void my_unexp () { throw 42; }

void
f () throw (std::bad_exception)
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
      return 0;
    }
  return 5;
}
