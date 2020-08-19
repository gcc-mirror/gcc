#include "analyzer-decls.h"

void test(int n)
{
  int i;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  for (i = 0; i < n; i++) {
      __analyzer_eval (i < n); /* { dg-warning "TRUE" } */
      /* (should report TRUE twice). */

      __analyzer_eval (i == 0); /* { dg-warning "TRUE" "1st" } */
      /* { dg-warning "FALSE" "2nd" { xfail *-*-* } .-1 } */
      /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-2 } */
      /* TODO(xfail^^^): ideally we ought to figure out i > 0 after 1st iteration.  */

      __analyzer_eval (i >= 0); /* { dg-warning "TRUE" } */

      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
  }

  __analyzer_eval (i >= n); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */

  __analyzer_eval (i == n); /* { dg-warning "TRUE" "true" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "unknown" { xfail *-*-* } .-1 } */
  /* TODO(xfail^^^): it only figures out i >= 256, rather than i == 256.  */

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
