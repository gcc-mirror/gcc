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
      __analyzer_dump_path (); /* { dg-message "path" } */
    }
  return problem;
}

/* As above, but call a static function.
   Even if the path to the call of called_by_test_6a is falsely rejected
   as infeasible, it still makes sense to complain about errors within
   the called function.  */

static void __attribute__((noinline))
called_by_test_6a (void *ptr)
{
  __builtin_free (ptr);
  __builtin_free (ptr); /* { dg-message "double-'free'" } */
}

int test_6a (int a, int b, void *ptr)
{
  int problem = 0;
  if (a)
    problem = 1;
  if (b)
    {
      if (!problem)
	problem = 2;
      called_by_test_6a (ptr);
    }
  return problem;
}

/* After state-merging, the shortest path skips the loop,
   but the shortest feasible path enters it.  */

void test_7 (int n)
{
  int entered_loop = 0;
  int i;
  for (i = 0; i < n; i++)
    entered_loop = 1;
  if (entered_loop)
    __analyzer_dump_path (); /* { dg-message "path" } */
}
