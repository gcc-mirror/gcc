#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct io_error {};
struct value_error {};
struct runtime_error {};

extern void do_something (); 

int test ()
{
  try
    {
      do_something ();
    }
  catch (const io_error &err)
    {
      __analyzer_dump_path (); // { dg-message "path" }
      return 1;
    }
  catch (const value_error &err)
    {
      __analyzer_dump_path (); // { dg-message "path" }
      return 2;
    }
  catch (const runtime_error &err)
    {
      __analyzer_dump_path (); // { dg-message "path" }
      return 3;
    }
  __analyzer_dump_path (); // { dg-message "path" }
  return 0;
}
