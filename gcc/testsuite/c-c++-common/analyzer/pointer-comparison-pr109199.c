#include "analyzer-decls.h"

void
test (void)
{
  int *b[1] = {};
  int **c = &b[0];
  __analyzer_eval((((c) + 1) == ((&b[0]) + 1))); /* { dg-warning "TRUE" "PR analyzer/109199" } */
}
