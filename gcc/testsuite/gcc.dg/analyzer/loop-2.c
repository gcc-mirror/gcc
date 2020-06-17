/* { dg-additional-options "-fno-analyzer-state-purge" } */
#include "analyzer-decls.h"

struct s
{
  int i;
};

void test(void)
{
  struct s s;

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */


  for (s.i=0; s.i<256; s.i++) {
    __analyzer_eval (s.i < 256); /* { dg-warning "TRUE" } */
      /* (should report TRUE twice). */

    __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */

    //__analyzer_eval (s.i == 0); /* { d-todo-g-warning "UNKNOWN" "" { xfail *-*-* } } */
      /* { d-todo-g-warning "TRUE" "" { target *-*-* } .-1 } */
      /* TODO(xfail^^^): we're only capturing the first iteration, so
	 we erroneously get i == 0.  */

      //__analyzer_eval (s.i >= 0); /* { d-todo-g-warning "TRUE" } */
  }

  __analyzer_eval (s.i >= 256); /* { dg-warning "TRUE" } */

  __analyzer_eval (s.i == 256); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-warning "UNKNOWN" "status quo" { target *-*-* } .-1 } */
  /* TODO(xfail^^^): ideally it should figure out i == 256 at exit.  */

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
