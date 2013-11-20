/* This checks the availability of the XL compiler intrinsics for
   transactional execution with the expected prototypes.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

#include <htmxlintrin.h>

int a = 0;
unsigned long g;

int
foo ()
{
  struct __htm_tdb *tdb_struct;
  void * const tdb = tdb_struct;
  long result;
  unsigned char code;

  result = __TM_simple_begin ();
  result = __TM_begin (tdb);
  result = __TM_end ();
  __TM_abort ();
  __TM_named_abort (42);
  __TM_non_transactional_store (&g, 42);
  result = __TM_nesting_depth (tdb);

  result = __TM_is_user_abort (tdb);
  result = __TM_is_named_user_abort (tdb, &code);
  result = __TM_is_illegal (tdb);
  result = __TM_is_footprint_exceeded (tdb);
  result = __TM_is_nested_too_deep (tdb);
  result = __TM_is_conflict (tdb);
  result = __TM_is_failure_persistent (result);
  result = __TM_failure_address (tdb);
  result = __TM_failure_code (tdb);
}
