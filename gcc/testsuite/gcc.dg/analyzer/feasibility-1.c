#include "analyzer-decls.h"

void test_1 (void)
{
  __analyzer_dump_path (); /* { dg-message "path" } */
}

void test_2 (int flag)
{
  if (flag)
    __analyzer_dump_path (); /* { dg-message "path" } */
}

void test_3 (int flag)
{
  if (flag)
    if (!flag)
      __analyzer_dump_path (); /* { dg-bogus "path" } */
}

int global_for_test_4;
static void __attribute__((noinline)) called_by_test_4 () {}
void test_4 (void)
{
  /* Verify that a state change that happens in a stmt that
     isn't the first within its BB can affect path feasibility.  */
  global_for_test_4 = 0;
  global_for_test_4 = 1;
  /* Thwart the optimizer.  */
  called_by_test_4 ();
  if (global_for_test_4)
    __analyzer_dump_path (); /* { dg-message "path" } */
}

/* Verify that loops don't confuse the feasibility checker.  */

void test_5 (void)
{
  for (int i = 0; i < 1024; i++)
    {
    }
  __analyzer_dump_path (); /* { dg-message "path" } */
}

/* Reproducer for an issue seen with CVE-2005-1689 (PR analyzer/96374): if we
   take the shortest path and update state and check feasibility per-edge, we
   can erroneously reject valid diagnostics.  */

int test_6 (int a, int b)
{
  int problem = 0;
  if (a)
    problem = 1;
  if (b)
    {
      if (!problem)
	problem = 2;
      __analyzer_dump_path (); /* { dg-message "path" "" { xfail *-*-* } } */
      /* XFAIL is PR analyzer/96374.  */
    }
  return problem;
}
