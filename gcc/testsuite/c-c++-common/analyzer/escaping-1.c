#include "../../gcc.dg/analyzer/analyzer-decls.h"

extern void unknown_fn (void *);

static int only_used_by_test_1;

static void test_1 (void)
{
  int local_1, local_2;
  __analyzer_dump_escaped (); /* { dg-warning "escaped: 0: " } */

  unknown_fn (NULL);
  __analyzer_dump_escaped (); /* { dg-warning "escaped: 0: " } */

  unknown_fn (&local_1);
  __analyzer_dump_escaped (); /* { dg-warning "escaped: 1: 'local_1'" } */

  /* Should be idempotent.  */
  unknown_fn (&local_1);
  __analyzer_dump_escaped (); /* { dg-warning "escaped: 1: 'local_1'" } */

  /* Escape a static global.  */
  unknown_fn (&only_used_by_test_1);
  __analyzer_dump_escaped (); /* { dg-warning "escaped: 2: 'local_1', 'only_used_by_test_1'" } */
}
