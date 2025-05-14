/* Verify that we detect a leak when unwinding multiple frames.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct io_error {};
struct value_error {};
struct runtime_error {};

int inner (int flag)
{
  if (flag)
    throw value_error (); // { dg-warning "leak" }
  // { dg-message "throwing exception of type 'value_error' here\.\.\." "" { target *-*-* } .-1 }

  return 0;
}

int __analyzer_middle (int flag)
{
  void *ptr = __builtin_malloc (1024); // { dg-message "allocated here" }

  int rval = inner (flag);

  __builtin_free (ptr);

  return rval; 
}

int outer ()
{
  try
    {
      __analyzer_middle (1);
    }
  catch (const io_error &err)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return 1;
    }
  catch (const value_error &err)
    {
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
