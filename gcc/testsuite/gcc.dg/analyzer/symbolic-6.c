#include "analyzer-decls.h"

int a[1024];
int b[1024];

extern void escape (void *ptr);

void test_1 (int *p)
{
  int c, d;
  escape (&c);

  *p = 42;
  __analyzer_eval (*p == 42); /* { dg-warning "TRUE" } */

  /* These writes shouldn't affect *p.  */
  c = 33;
  d = 44;
  __analyzer_eval (*p == 42); /* { dg-warning "TRUE" } */

  /* This write could affect *p.  */
  a[16] = 55;
  __analyzer_eval (*p == 42); /* { dg-warning "UNKNOWN" } */
}
