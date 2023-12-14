#include "analyzer-decls.h"

void test (int *p, int i, int j)
{
  p[3] = 42;
  __analyzer_eval (p[3] == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (*(p + 3) == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[i] == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[j] == 42); /* { dg-warning "UNKNOWN" } */

  //__analyzer_dump ();

  p[i] = 17;

  //__analyzer_dump ();

  __analyzer_eval (p[3] == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (p[i] == 17); /* { dg-warning "TRUE" } */
  __analyzer_eval (p[j] == 17); /* { dg-warning "UNKNOWN" } */
}
