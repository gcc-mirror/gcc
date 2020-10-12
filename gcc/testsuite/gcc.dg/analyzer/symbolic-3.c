#include "analyzer-decls.h"

int iarr[16];

void test_1 (int i, int j)
{
  int init_el_8 = iarr[8];
  __analyzer_eval (init_el_8 == iarr[8]); /* { dg-warning "TRUE" } */

  iarr[i] = j;
  __analyzer_eval (init_el_8 == iarr[8]); /* { dg-warning "UNKNOWN" } */
}
