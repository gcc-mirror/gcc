#include <stdlib.h>
#include "analyzer-decls.h"

static void ensure_equal (int a, int b)
{
  if (a != b)
    abort ();
}

void test(int i, int j)
{
  __analyzer_eval (i == j); /* { dg-warning "UNKNOWN" } */

  ensure_equal (i, j);

  __analyzer_eval (i == j); /* { dg-warning "TRUE" } */
}
