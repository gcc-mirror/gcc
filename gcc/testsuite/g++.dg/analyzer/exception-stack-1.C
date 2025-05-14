#include "../../gcc.dg/analyzer/analyzer-decls.h"

void test (void)
{
  try
    {
      try
	{
	  throw 42;
	  __analyzer_dump_path (); // { dg-bogus "path" }
	}
      catch (...)
	{
	  try
	    {
	      throw 1066; // throw an inner exception
	      __analyzer_dump_path (); // { dg-bogus "path" }
	    }
	  catch (int i)
	    {
	      __analyzer_eval (i == 1066); // { dg-warning "TRUE" }
	    }
	  throw; // rethrow the outer exception
	  __analyzer_dump_path (); // { dg-bogus "path" }
	}
      __analyzer_dump_path (); // { dg-bogus "path" }
    }
  catch (int j)
    {
      __analyzer_eval (j == 42); // { dg-warning "TRUE" }
      __analyzer_dump_path (); // { dg-message "path" }
      throw;
    }    
  __analyzer_dump_path (); // { dg-bogus "path" }
}
