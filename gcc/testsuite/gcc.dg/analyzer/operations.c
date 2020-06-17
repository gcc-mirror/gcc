#include "analyzer-decls.h"

void test (int i, int j)
{
  int k, m;

  if (i > 42) {
    __analyzer_eval (i > 42); /* { dg-warning "TRUE" } */

    i += 3;

    __analyzer_eval (i > 45); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
    /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
    /* TODO(xfail): do we really know this?  what about overflow?  */

    i -= 1;

    __analyzer_eval (i > 44); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
    /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
    /* TODO(xfail): do we really know this?  what about overflow?  */

    i = 3 * i;

    __analyzer_eval (i > 132); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
    /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
    /* TODO(xfail): do we really know this?  what about overflow?  */

    i /= 2;

    __analyzer_eval (i > 66); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
    /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
    /* TODO(xfail): do we really know this?  what about overflow?  */

    /* We don't know anything about j, so we don't know anything about k: */
    k = i + j;
    __analyzer_eval (k == 0); /* { dg-warning "UNKNOWN" } */

    /* However, we should now know that m > 67: */
    m = i + 1;
    __analyzer_eval (m > 67); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
    /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
    /* TODO(xfail): do we really know this?  what about overflow?  */
  }
}
