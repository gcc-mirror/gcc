#include "analyzer-decls.h"

static int __attribute__((noipa))
negate_int (int x)
{
  return -x;
}

void
test_1 (int a)
{
  __analyzer_eval (a + negate_int (a) == 0); /* { dg-warning "TRUE" } */
}
