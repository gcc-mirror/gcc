// Testing exception specifications.
// Test 3: the bad_exception throw succeeds.

#include <stdlib.h>
#include <exception>

void my_term ()  { exit (1); }
void my_unexp () { throw 42; }

void
f () throw (bad_exception)
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
      return 0;
    }
  return 5;
}
