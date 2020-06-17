/* { dg-additional-options "-fanalyzer-transitivity" } */

#include "analyzer-decls.h"

void test (int i)
{
  switch (i)
    {
    case 0:
      __analyzer_eval (i == 0); /* { dg-warning "TRUE" } */
      break;

    case 3 ... 5:
      __analyzer_eval (i >= 3); /* { dg-warning "TRUE" } */
      __analyzer_eval (i <= 5); /* { dg-warning "TRUE" } */
      break;

    default:
      __analyzer_eval (i == 0); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 2); /* { dg-warning "UNKNOWN" } */
      __analyzer_eval (i == 3); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 4); /* { dg-warning "FALSE" "desired" { xfail *-*-* } } */
      /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
      /* TODO(xfail^^^): we're only checking against endpoints of case
	 ranges, not the insides.  */
      __analyzer_eval (i == 5); /* { dg-warning "FALSE" } */
      __analyzer_eval (i == 6); /* { dg-warning "UNKNOWN" } */
      break;
    }
}
