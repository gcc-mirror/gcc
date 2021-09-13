#include "analyzer-decls.h"

static int __analyzer_called_function(int j)
{
  int k;

  __analyzer_eval (j > 4); /* { dg-warning "TRUE" } */

  k = j - 1;

  __analyzer_eval (k > 3); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  /* TODO(xfail): we're not then updating based on the assignment.  */

  return k;
}

void test(int i)
{
  __analyzer_eval (i > 4); /* { dg-warning "UNKNOWN" } */

  if (i > 4) {

    __analyzer_eval (i > 4); /* { dg-warning "TRUE" } */

    i = __analyzer_called_function(i);

    __analyzer_eval (i > 3); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
    /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
    /* TODO(xfail): we're not updating from the returned value.  */
  }

  __analyzer_eval (i > 3); /* { dg-warning "UNKNOWN" } */
}
