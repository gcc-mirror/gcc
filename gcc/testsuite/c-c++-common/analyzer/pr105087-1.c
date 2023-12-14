#include "analyzer-decls.h"

extern void *inner_alloc (void);

void * __attribute__((noinline))
outer_alloc (void)
{
  return inner_alloc ();
}

void test_1 (void)
{
  void *p, *q;

  p = outer_alloc ();
  q = outer_alloc ();
  __analyzer_eval (p == q); /* { dg-warning "UNKNOWN" } */
}
