#include "filter1.h"

struct e1 {};
struct e2 {};

void
ex_test ()
{
  a aa;
  try
    {
      throw e1 ();
    }
  catch (e2 &)
    {
    }
}
