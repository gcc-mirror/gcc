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

main ()
{
  set_terminate (my_term);
  set_unexpected (my_unexp);

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
  catch (bad_exception)
    {
      return 4;
    }
  return 5;
}
