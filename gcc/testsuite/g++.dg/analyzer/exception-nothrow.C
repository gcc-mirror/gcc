#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void do_something () __attribute__((nothrow));

/* A wrapper function to stop the try/catch being optimized away.  */

void wrapper () __attribute__((noinline));
void wrapper ()
{
  do_something ();
}

int test ()
{
  try
    {
      wrapper ();
    }
  catch (int i)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return 1;
    }
  __analyzer_dump_path (); // { dg-message "path" }
  return 0;
}
