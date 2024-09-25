/* { dg-additional-options "-fno-analyzer-call-summaries -fanalyzer-transitivity" } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>
#include "analyzer-decls.h"

extern int foo (int);

static int __attribute__((noinline))
do_stuff (int *p, int n)
{
  int sum = 0;
  int i;
  for (i = 0; i < n; i++)
    p[i] = i; /* { dg-warning "dereference of possibly-NULL 'p'" } */
  for (i = 0; i < n; i++)
    sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
  return sum;
}

static int __attribute__((noinline))
do_stuff_2 (int *p, int n)
{
  return 0;
}

/* Various examples of functions that use either a malloc buffer
   or a local buffer, do something, then conditionally free the
   buffer, tracking whether "free" is necessary in various
   ways.

   In each case, there ought to be only two paths through the function,
   not four.  */

/* Repeated (n > 10) predicate.  */

int test_repeated_predicate_1 (int n)
{
  int buf[10];
  int *ptr;
  int result;

  if (n > 10)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  result = do_stuff (ptr, n);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */


  if (n > 10)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}

/* A simpler version of the above.  */

int test_repeated_predicate_2 (int n)
{
  int buf[10];
  int *ptr;
  int result;

  if (n > 10)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  result = do_stuff_2 (ptr, n);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  if (n > 10)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}

/* A predicate that sets a flag for the 2nd test.  */

int test_explicit_flag (int n)
{
  int buf[10];
  int *ptr;
  int result;
  int need_to_free = 0;

  if (n > 10)
    {
      ptr = (int *)malloc (sizeof (int) * n);
      need_to_free = 1;
    }
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  result = do_stuff (ptr, n);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */


  if (need_to_free)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}

/* Pointer comparison.  */

int test_pointer_comparison (int n)
{
  int buf[10];
  int *ptr;
  int result;

  if (n > 10)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  result = do_stuff (ptr, n);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */


  if (ptr != buf)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}

/* Set a flag based on a conditional, then use it, then reuse the
   conditional.  */

int test_initial_flag (int n)
{
  int buf[10];
  int *ptr;
  int result;
  int on_heap = 0;

  if (n > 10)
    on_heap = 1;
  else
    on_heap = 0;

  /* Due to state-merging, we lose the relationship between 'n > 10'
     and 'on_heap' here; we have to rely on feasibility-checking
     in the diagnostic_manager to reject the false warnings.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  if (on_heap)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  result = do_stuff (ptr, n);

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */
  // FIXME: why 3 here?

  if (n > 10)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}
