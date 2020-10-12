#include "analyzer-decls.h"

void test(int start, int end, int step)
{
  int i;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  for (i = start; i < end; i += step) {
      __analyzer_eval (i < end); /* { dg-warning "TRUE" "true" } */
      /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
      /* TODO(xfail^^^): should report TRUE twice. */

      __analyzer_eval (i == start); /* { dg-warning "TRUE" "1st" } */
      /* { dg-warning "FALSE" "2nd" { xfail *-*-* } .-1 } */
      /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-2 } */
      /* TODO(xfail^^^): ideally we ought to figure out i > 0 after 1st iteration.  */

      /* We don't know the direction of step.  */
      __analyzer_eval (i >= start); /* { dg-warning "TRUE" "true" } */
      /* { dg-warning "UNKNOWN" "unknown" { target *-*-* } .-1 } */

      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  }

  // FIXME: do we know this?  What about direction of step?
  __analyzer_eval (i >= end); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */

  // FIXME: do we know this?  What if we overshoot?
  __analyzer_eval (i == end); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  /* TODO(xfail^^^): it only figures out i >= 256, rather than i == 256.  */

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
