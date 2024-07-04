/* Verify that we consider various paths to be impossible,
   using functions to thwart early optimizations.  */

#include "analyzer-decls.h"

void test_1 (int idx)
{
  if (idx > 0)
    if (idx - 1 < 0)
      __analyzer_dump_path (); /* { dg-bogus "" } */
}

static int called_by_test_1a (int idx)
{
  return idx - 1;
}

void test_1a (int idx)
{
  if (idx > 0)
    if (called_by_test_1a (idx) < 0)
      __analyzer_dump_path (); /* { dg-bogus "" } */
}

void test_2 (int idx)
{
  if (idx + 1 > 0)
    if (idx < 0)
      __analyzer_dump_path (); /* { dg-bogus "" } */
}

static int called_by_test_2a (int idx)
{
  return idx + 1;
}

void test_2a (int idx)
{
  if (called_by_test_2a (idx) > 0)
    if (idx < 0)
      __analyzer_dump_path (); /* { dg-bogus "" } */
}
