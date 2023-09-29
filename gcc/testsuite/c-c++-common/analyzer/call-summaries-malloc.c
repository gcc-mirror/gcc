/* { dg-additional-options "-fanalyzer-call-summaries --param analyzer-min-snodes-for-call-summary=0" } */

/* There need to be at least two calls to a function for the
   call-summarization code to be used.
   TODO: add some kind of test that summarization *was* used.  */

#include <stdlib.h>
#include <string.h>
#include "../../gcc.dg/analyzer/analyzer-decls.h"

int *malloc_int (int i)
{
  int *res = (int *) malloc (sizeof (int));
  if (!res)
    return NULL;
  *res = i;
  return res;
}

void test_malloc_int (int x)
{
  int *p, *q;

  p = malloc_int (42);
  if (p)
    __analyzer_eval (*p == 42); /* { dg-warning "TRUE" } */
  free (p);

  q = malloc_int (x);
  if (q)
    __analyzer_eval (*q == x); /* { dg-warning "TRUE" } */
  free (q);
}

void test_leak (int x)
{
  int *p = malloc_int (x); /* { dg-message "when 'malloc_int' returns pointer to heap-allocated buffer" "" { target c } } */
  /* { dg-message "when 'int\\* malloc_int\\(int\\)' returns pointer to heap-allocated buffer" "" { target c++ } .-1 } */
} /* { dg-message "leak of 'p'" } */

void *wrapped_malloc (size_t sz)
{
  return malloc (sz);
}

void wrapped_free (void *p)
{
  free (p);
}

void test_wrapped_malloc_and_free (size_t sz)
{
  void *p = wrapped_malloc (100);
  void *q = wrapped_malloc (sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(\[^\n\r\]*\\)100'" } */
  __analyzer_dump_capacity (q); /* { dg-warning "capacity: 'INIT_VAL\\(sz_\[^\n\r\]*\\)'" } */
  wrapped_free (p);
  wrapped_free (q);
}

void test_use_after_free (void)
{
  // TODO
}

void test_use_without_check (size_t sz)
{
  char *buf = (char *) wrapped_malloc (sz); /* { dg-message "this call could return NULL" } */
  memset (buf, 'x', sz); /* { dg-warning "use of possibly-NULL 'buf' where non-null expected" } */
  wrapped_free (buf);
}

void test_out_of_bounds (size_t sz)
{
  char *buf = (char *) wrapped_malloc (sz);
  if (!buf)
    return;
  memset (buf, 'x', sz);
  buf[sz] = '\0'; /* { dg-warning "heap-based buffer overflow" } */
  wrapped_free (buf);
}
