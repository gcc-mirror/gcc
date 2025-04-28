#include "../../gcc.dg/analyzer/analyzer-decls.h"

int test ()
{
  try
    {
      throw 42;
    }
  catch (...)
    {
      throw;
    }
}
