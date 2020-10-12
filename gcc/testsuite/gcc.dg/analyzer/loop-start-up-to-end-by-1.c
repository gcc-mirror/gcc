#include "analyzer-decls.h"

void test(int start, int end)
{
  int i;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  for (i = start; i < end; i++) {
      __analyzer_eval (i < end); /* { dg-warning "TRUE" "true" } */

      __analyzer_eval (i == start); /* { dg-warning "TRUE" "1st" } */
      /* { dg-warning "FALSE" "2nd" { xfail *-*-* } .-1 } */
      /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-2 } */
      /* TODO(xfail^^^): ideally we ought to figure out i > 0 after 1st iteration.  */

      __analyzer_eval (i >= start); /* { dg-warning "TRUE" "true" } */
      /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
      /* TODO(xfail^^^): should report TRUE twice. */

      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  }

  __analyzer_eval (i >= end); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */

  __analyzer_eval (i == end); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  /* TODO(xfail^^^): it only figures out i >= end, rather than i == end.  */

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
