/* { dg-additional-options "-fanalyzer-checker=taint" } */
// TODO: remove need for this option

#include "analyzer-decls.h"

int v_start;

__attribute__((tainted_args))
void test (int v_tainted, int v_has_lb, int v_has_ub, int v_stop)
{
  /* Get each var into the 5 different taintedness states.  */
  if (v_has_lb < 10)
    return;
  if (v_has_ub > 100)
    return;
  if (v_stop < 0 || v_stop > 100)
    return;

  /* Verify that we have the taintedness states we expect.  */

  __analyzer_dump_state ("taint", v_start); /* { dg-warning "state: 'start'" } */
  __analyzer_dump_state ("taint", v_tainted); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_has_lb); /* { dg-warning "state: 'has_lb'" } */
  __analyzer_dump_state ("taint", v_has_ub); /* { dg-warning "state: 'has_ub'" } */
  __analyzer_dump_state ("taint", v_stop); /* { dg-warning "state: 'stop'" } */

  /* Check all combinations of taintedness state.  */
  __analyzer_dump_state ("taint", v_start + v_start); /* { dg-warning "state: 'start'" } */
  __analyzer_dump_state ("taint", v_start + v_tainted); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_start + v_has_lb); /* { dg-warning "state: 'has_lb'" } */
  __analyzer_dump_state ("taint", v_start + v_has_ub); /* { dg-warning "state: 'has_ub'" } */
  __analyzer_dump_state ("taint", v_start + v_stop); /* { dg-warning "state: 'stop'" } */

  __analyzer_dump_state ("taint", v_tainted + v_start); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_tainted + v_tainted); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_tainted + v_has_lb); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_tainted + v_has_ub); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_tainted + v_stop); /* { dg-warning "state: 'tainted'" } */

  __analyzer_dump_state ("taint", v_has_lb + v_start); /* { dg-warning "state: 'has_lb'" } */
  __analyzer_dump_state ("taint", v_has_lb + v_tainted); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_has_lb + v_has_lb); /* { dg-warning "state: 'has_lb'" } */
  __analyzer_dump_state ("taint", v_has_lb + v_has_ub); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_has_lb + v_stop); /* { dg-warning "state: 'has_lb'" } */

  __analyzer_dump_state ("taint", v_has_ub + v_start); /* { dg-warning "state: 'has_ub'" } */
  __analyzer_dump_state ("taint", v_has_ub + v_tainted); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_has_ub + v_has_lb); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_has_ub + v_has_ub); /* { dg-warning "state: 'has_ub'" } */
  __analyzer_dump_state ("taint", v_has_ub + v_stop); /* { dg-warning "state: 'has_ub'" } */

  __analyzer_dump_state ("taint", v_stop + v_start); /* { dg-warning "state: 'stop'" } */
  __analyzer_dump_state ("taint", v_stop + v_tainted); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", v_stop + v_has_lb); /* { dg-warning "state: 'has_lb'" } */
  __analyzer_dump_state ("taint", v_stop + v_has_ub); /* { dg-warning "state: 'has_ub'" } */
  __analyzer_dump_state ("taint", v_stop + v_stop); /* { dg-warning "state: 'stop'" } */
}
