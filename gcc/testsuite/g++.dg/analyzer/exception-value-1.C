/* Verify that we can access values in exceptions.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

int test ()
{
    try
    {
      throw 42; // { dg-message "\\(1\\) throwing exception of type 'int' here..." }
      __analyzer_dump_path (); // { dg-bogus "path" }
    }
    catch (int i) // { dg-message "\\(2\\) \.\.\.catching exception of type 'int' here" }
    {
      __analyzer_dump_path (); // { dg-message "path" }
      __analyzer_eval (i == 42); // { dg-warning "TRUE" }
      return 1;
    }
    __analyzer_dump_path (); // { dg-bogus "path" }
    return 0;
}
