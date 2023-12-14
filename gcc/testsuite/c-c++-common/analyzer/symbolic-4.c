#include <string.h>
#include "analyzer-decls.h"

void test_1 (int i, int j, int k)
{
  int iarr[16];
  iarr[i] = j;
  __analyzer_eval (iarr[i] == j); /* { dg-warning "TRUE" } */
  __analyzer_eval (iarr[k] == j); /* { dg-warning "UNKNOWN" } */

  memset (iarr, 0, sizeof (iarr));
  __analyzer_eval (iarr[0] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (iarr[i] == 0); /* { dg-warning "TRUE" } */
  __analyzer_eval (iarr[i] == j); /* { dg-warning "UNKNOWN" } */

  iarr[i] = j;
  __analyzer_eval (iarr[i] == j); /* { dg-warning "TRUE" } */
  __analyzer_eval (iarr[0] == 0); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (iarr[i] == 0); /* { dg-warning "UNKNOWN" } */
}
