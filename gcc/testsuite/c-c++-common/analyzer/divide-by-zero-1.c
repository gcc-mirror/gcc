#include "analyzer-decls.h"

static int __attribute__((noipa))
return_zero (void)
{
  return 0;
}

void
test_div (int a)
{
  __analyzer_eval (a / return_zero () == 0); /* { dg-warning "UNKNOWN" } */
}

void
test_mod (int a)
{
  __analyzer_eval (a % return_zero () == 0); /* { dg-warning "UNKNOWN" } */
}
