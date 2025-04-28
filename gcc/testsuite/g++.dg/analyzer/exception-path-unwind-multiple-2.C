/* Verify that we follow the correct paths when we know the typeinfo of
   an exception: interprocedural case where unwind multiple frame,
   failing to match the type.  */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct io_error {};
struct value_error {};
struct runtime_error {};

int inner (int flag)
{
  if (flag)
    throw value_error (); // { dg-message "throwing exception of type 'value_error' here..." }
  // { dg-message "unwinding 2 stack frames" "" { target *-*-* } .-1 }

  return 0;
}

int middle (int flag)
{
  try
    {
      return inner (flag);
    }
  catch (const io_error &err) // this shouldn't be matched
    {
      return -1;
    }
}

int outer ()
{
  try
    {
      middle (1);
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
