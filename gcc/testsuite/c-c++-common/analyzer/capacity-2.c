/* { dg-skip-if "" { powerpc*-*-aix* } } */
/* { dg-skip-if "requires hosted libstdc++ for stdlib size_t" { ! hostedlib } } */

#include <stdlib.h>
#include "analyzer-decls.h"

extern void might_realloc (void *);
extern void cant_realloc (const void *);

void *
test_realloc_1 (void *p, size_t new_sz)
{
  void *q = realloc (p, new_sz);
  __analyzer_dump_capacity (q); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" "failure" } */
  /* { dg-warning "capacity: 'INIT_VAL\\(new_sz\[^\n\r\]*\\)'" "success" { target *-*-* } .-1 } */
  return q;
}

void *
test_realloc_2 (size_t sz_a, size_t sz_b)
{
  void *p = malloc (sz_a);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(sz_a_\[^\n\r\]*\\)'" } */
  void *q = realloc (p, sz_b);
  __analyzer_dump_capacity (q); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" "failure" } */
  /* { dg-warning "capacity: 'INIT_VAL\\(sz_b\[^\n\r\]*\\)'" "success" { target *-*-* } .-1 } */
  return q; /* { dg-warning "leak of 'p'" } */
}

void *
test_might_realloc (void)
{
  void *p = malloc (1024);

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(size_t\\)1024'" } */

  might_realloc (p);

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" } */

  return p;
}

void *
test_cant_realloc (void)
{
  void *p = malloc (1024);

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(size_t\\)1024'" } */

  cant_realloc (p);

  __analyzer_dump_capacity (p); /* { dg-warning "capacity: '\\(size_t\\)1024'" } */

  return p;
}


