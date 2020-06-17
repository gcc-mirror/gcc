#include <stdlib.h>
#include "analyzer-decls.h"

extern int foo (int);

static int __attribute__((noinline))
do_stuff_2 (int *p, int n)
{
  return 0;
}

/* As malloc-vs-local.c, but hand-inlining the logic.  */

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

  {
    int *p = ptr;
    int sum = 0;
    int i;
    for (i = 0; i < n; i++)
      p[i] = i;
    for (i = 0; i < n; i++)
      sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
    result = sum;
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */

  if (n > 10)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}

/* As above, but with just one loop.  */

int test_repeated_predicate_1a (int n)
{
  int buf[10];
  int *ptr;
  int result;

  if (n > 10)
    ptr = (int *)malloc (sizeof (int) * n);
  else
    ptr = buf;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

  {
    int *p = ptr;
    int sum = 0;
    int i;
    for (i = 0; i < n; i++)
      p[i] = i;
    result = sum;
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */

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

  {
    int *p = ptr;
    int sum = 0;
    int i;
    for (i = 0; i < n; i++)
      p[i] = i;
    for (i = 0; i < n; i++)
      sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
    result = sum;
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */

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

  {
    int *p = ptr;
    int sum = 0;
    int i;
    for (i = 0; i < n; i++)
      p[i] = i;
    for (i = 0; i < n; i++)
      sum += foo (p[i]); /* { dg-bogus "uninitialized" } */
    result = sum;
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */

  if (ptr != buf)
    free (ptr); /* { dg-bogus "not on the heap" } */

  return result; /* { dg-bogus "leak" } */
}
