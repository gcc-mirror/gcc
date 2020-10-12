#include "analyzer-decls.h"

struct foo
{
  int ival;
  int iarr[10];
};

void test_1 (int i, int j)
{
  struct foo fooarr[4];
  fooarr[1].ival = 42;
  fooarr[1].iarr[3] = 27;
  fooarr[2].iarr[1] = 17;
  __analyzer_eval (fooarr[1].ival == 42); /* { dg-warning "TRUE" } */
  __analyzer_eval (fooarr[1].iarr[3] == 27); /* { dg-warning "TRUE" } */
  __analyzer_eval (fooarr[2].iarr[1] == 17); /* { dg-warning "TRUE" } */

  /* Symbolic binding.  */
  fooarr[2].iarr[i] = j;
  __analyzer_eval (fooarr[2].iarr[i] == j); /* { dg-warning "TRUE" } */

  /* We should have lost our knowledge about fooarr[2].
     It's not clear to me if we should also lose our knowledge about
     fooarr[1] (for the case where i is negative).  For now, we do.  */
  __analyzer_eval (fooarr[1].ival == 42); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (fooarr[1].iarr[3] == 27); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (fooarr[2].iarr[1] == 17); /* { dg-warning "UNKNOWN" } */
  /* Should also be safe to read from fooarr[2];
     it isn't known to be uninit anymore.  */
  __analyzer_eval (fooarr[2].iarr[10] == 17); /* { dg-warning "UNKNOWN" } */
}
