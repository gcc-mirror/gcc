#include "../../gcc.dg/analyzer/analyzer-decls.h"

int test ()
{
  try
    {
      throw 42; // { dg-message "throwing exception of type 'int' here\.\.\." }
    }
  catch (int i) // { dg-message "\.\.\.catching exception of type 'int' here" }
    {
      __analyzer_dump_path (); // { dg-message "path" }
      __analyzer_eval (i == 42); // { dg-warning "TRUE" }
      return -2;
    }
  catch (...)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return -1;
    }
  __analyzer_dump_path (); // { dg-bogus "path" }
  return 0;
}
