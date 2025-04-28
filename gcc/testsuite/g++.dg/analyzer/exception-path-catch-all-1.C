#include "../../gcc.dg/analyzer/analyzer-decls.h"

int test ()
{
  try
    {
      throw 42;
    }
  catch (...)
    {
      __analyzer_dump_path ();
      return -1;
    }
  __analyzer_dump_path ();
  return 0;
}
