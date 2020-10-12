#include "analyzer-decls.h"

int a[1024];
int b[1024];

extern void escape (void *ptr);

void test_1 (int *p)
{
  int c, d;
  escape (&c);
  a[16] = 42;
  b[16] = 17;
  c = 33;
  d = 44;
  __analyzer_eval (a[16] == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (b[16] == 17); /* { dg-warning "TRUE" } */
  __analyzer_eval (c == 33); /* { dg-warning "TRUE" } */
  __analyzer_eval (d == 44); /* { dg-warning "TRUE" } */

  /* Write through an externally-provided pointer.  */
  *p = 100;
  /* It could clobber our writes to the global arrays...  */
  __analyzer_eval (a[16] == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (b[16] == 17); /* { dg-warning "UNKNOWN" } */  
  /* ...but can't clobber locals, even ones like "c" that have escaped.  */
  __analyzer_eval (c == 33); /* { dg-warning "TRUE" } */
  __analyzer_eval (d == 44); /* { dg-warning "TRUE" } */
}
