#include "../../gcc.dg/analyzer/analyzer-decls.h"

/* Top-level rethrow (PR analyzer/123880).  */

void test_1 ()
{
    throw;
}

/* Intraprocedural leak involving a rethrow.  */

void test_2 ()
{
  void *p = __builtin_malloc (1024);
  throw; // { dg-warning "leak of 'p'" }
  // { dg-message "rethrowing exception here\.\.\." "rethrow event" { target *-*-* } .-1 }
}

/* Interprocedural leak involving a rethrow.  */

static void called_by_test_3 ()
{
  throw; // { dg-warning "leak of 'p'" }
  // { dg-message "rethrowing exception here\.\.\." "rethrow event" { target *-*-* } .-1 }
}

void test_3 ()
{
  void *p = __builtin_malloc (1024); // { dg-message "allocated here" }
  called_by_test_3 ();
}

/* Rethrow of a rethrow.  */

void test_4 ()
{
  try
    {
      throw;
    }
  catch (...)
    {
      __analyzer_dump_path (); // { dg-message "path" "" { xfail *-*-* } }
      throw;
    }
}
