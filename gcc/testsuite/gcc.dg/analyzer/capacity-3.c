/* { dg-require-effective-target alloca } */

#include <stdlib.h>
#include "analyzer-decls.h"

static void __attribute__((noinline))
__analyzer_callee_1 (size_t inner_sz)
{
  void *p = __builtin_alloca (inner_sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(outer_sz_\[^\n\r\]*\\)'" } */
}

void
test_1 (int flag, size_t outer_sz)
{
  if (flag)
    __analyzer_callee_1 (outer_sz);

  /* Verify that we merge state; in particular, the dynamic size of "p"
     in the called frame should have been purged.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

void
test_2 (int flag, size_t sz)
{
  if (flag)
    {
      void *p = malloc (sz);
      __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(sz_\[^\n\r\]*\\)'" } */
      free (p);
      /* The dynamic size of "p" should have been purged.  */
      __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" } */
    }

  /* Verify that we merge state.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
/* Verify that we purge state on the NULL branch when a malloc result is
   tested against NULL.  */

void
test_3 (size_t sz)
{
  void *p = malloc (sz);

  if (p)
    {
      __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(sz_\[^\n\r\]*\\)'" } */
    }
  else
    {
      /* The dynamic size of "p" should have been purged
	 due to "p" being equal to NULL.  */
      __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" } */
    }

  free (p);
  
  /* The dynamic size of "p" should have been purged.  */
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'UNKNOWN\\(sizetype\\)'" } */

    /* Verify that we merge state.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}

/* Verify that we purge dynamic extent of a pointer when it leaks.  */

static void __attribute__((noinline))
__analyzer_callee_4 (size_t inner_sz)
{
  void *p = malloc (inner_sz);
  __analyzer_dump_capacity (p); /* { dg-warning "capacity: 'INIT_VAL\\(outer_sz_\[^\n\r\]*\\)'" } */
} /* { dg-warning "leak of 'p'" } */

void
test_4 (int flag, size_t outer_sz)
{
  if (flag)
    __analyzer_callee_4 (outer_sz);

  /* Verify that we merge state.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
