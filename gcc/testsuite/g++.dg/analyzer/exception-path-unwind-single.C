/* Verify that we follow the correct paths when we know the typeinfo of
   an exception: interprocedural case where we unwind two frames.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct io_error {};
struct value_error {};
struct runtime_error {};

int inner (int flag)
{
  if (flag)
    throw value_error (); // { dg-message "throwing exception of type 'value_error' here..." }

  return 0;
}

int outer ()
{
  try
    {
      inner (1);
    }
  catch (const io_error &err)
    {
      __analyzer_dump_path (); // { dg-bogus "path" }
      return 1;
    }
  catch (const value_error &err) // { dg-message "\.\.\.catching exception of type 'value_error' here" }
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

// TODO: test coverage for unwinding stack frame events
