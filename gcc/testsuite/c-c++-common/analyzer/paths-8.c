#include "analyzer-decls.h"

static void __attribute__((noinline))
__analyzer_callee_1 (void)
{
  /* empty.  */
}

void
test_1 (int flag)
{
  if (flag)
    __analyzer_callee_1 ();

  /* Verify that we merge state, whether or not the call happens.  */
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
