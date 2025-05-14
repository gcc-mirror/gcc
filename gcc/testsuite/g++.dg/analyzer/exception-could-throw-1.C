#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void do_something (); 
extern void do_something_nothrow () __attribute__ ((nothrow));; 

int test ()
{
  try
    {
      do_something ();
    }
  catch (int i)
    {
      int j = i;
      __analyzer_eval (i == 42); // { dg-warning "UNKNOWN" }
      __analyzer_eval (i == j); // { dg-warning "TRUE" }
      __analyzer_dump_path (); // { dg-message "path" }
      return 1;
    }
  __analyzer_dump_path (); // { dg-message "path" }
  return 0;
}

int test_nothrow ()
{
  try
    {
      do_something_nothrow ();
    }
  catch (int i)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return 1;
    }
  __analyzer_dump_path (); // { dg-message "path" }
  return 0;
}
