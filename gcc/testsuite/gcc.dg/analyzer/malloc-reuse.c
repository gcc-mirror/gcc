/* { dg-require-effective-target alloca } */

#include <stdlib.h>
#include "analyzer-decls.h"

/* Multiple calls to malloc (without a free) should return non-equal pointers.  */

void test_1 (void)
{
  void *p, *q;
  p = malloc (1024);
  if (!p)
    return;
  q = malloc (1024);
  __analyzer_eval (p == q); /* { dg-warning "FALSE" } */
  free (p);
  free (q);
}

/* Multiple calls to malloc with a free might or might not
   return the same pointer.  */

void test_2 (void)
{
  void *p, *q;
  p = malloc (1024);
  if (!p)
    return;
  free (p);
  
  q = malloc (1024);
  __analyzer_eval (p == q); /* { dg-warning "UNKNOWN" "ideal" { xfail *-*-* } } */
  /* { dg-bogus "FALSE" "status quo" { xfail *-*-* } .-1 } */
  // TODO: ideally this should be UNKNOWN
  free (q);
}

void test_two_malloc_sites_same_size (int flag)
{
  void *p;
  if (flag)
    p = malloc (1024);
  else
    p = malloc (1024);
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
  free (p);
}

void test_two_malloc_sites_different_sizes (int flag)
{
  void *p;
  if (flag)
    p = malloc (4096);
  else
    p = malloc (1024);
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  free (p);
}
