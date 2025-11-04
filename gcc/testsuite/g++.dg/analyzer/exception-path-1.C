/* { dg-additional-options "-fdiagnostics-add-output=sarif" } */

/* Verify that we follow the correct paths when we know the typeinfo of
   an exception.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct io_error {};
struct value_error {};
struct runtime_error {};

int test ()
{
  try
    {
      throw value_error (); // { dg-message "\\(1\\) throwing exception of type 'value_error' here..." }
      __analyzer_dump_path (); // { dg-bogus "path" }
    }
  catch (const io_error &err)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return 1;
    }
  catch (const value_error &err) // { dg-message "\\(2\\) \.\.\.catching exception of type 'value_error' here" }
    {
      __analyzer_dump_path (); // { dg-message "path" }
      return 2;
    }
  catch (const runtime_error &err)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return 3;
    }
  __analyzer_dump_path (); // { dg-bogus "path" }
  return 0;
}

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest exception-path-1.C "exception-path-1-sarif.py" } } */
