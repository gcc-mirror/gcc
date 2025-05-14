#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct io_error {};
struct value_error {};
struct runtime_error {};

void test (void)
{
  try
    {
      try
	{
	  throw value_error (); // { dg-message "\\(1\\) throwing exception of type 'value_error' here..." }
	  __analyzer_dump_path (); // { dg-bogus "path" }
	}
      catch (...) // { dg-message "\\(2\\) \.\.\.catching exception of type 'value_error' here" }
	{
	  try
	    {
	      throw io_error (); // { dg-message "\\(3\\) throwing exception of type 'io_error' here..." }
	      __analyzer_dump_path (); // { dg-bogus "path" }
	    }
	  catch (const io_error &err) // { dg-message "\\(4\\) \.\.\.catching exception of type 'io_error' here" }
	    {
	      /* discard it */
	    }

	  // rethrow the outer exception
	  throw; // { dg-message "\\(5\\) rethrowing exception of type 'value_error' here..." }
	  __analyzer_dump_path (); // { dg-bogus "path" }
	}
      __analyzer_dump_path (); // { dg-bogus "path" }
    }
  catch (const value_error &err) // { dg-message "\\(6\\) \.\.\.catching exception of type 'value_error' here" }
    {
      __analyzer_dump_path (); // { dg-message "path" }
      throw;
    }
  catch (...)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
    }      
  __analyzer_dump_path (); // { dg-bogus "path" }
}
