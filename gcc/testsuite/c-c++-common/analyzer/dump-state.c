/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

/* Verify that __analyzer_dump_state works as expected.  */

#include <stdlib.h>
#include "analyzer-decls.h"

void test_1 (void)
{
  void *p = malloc (1024);
  __analyzer_dump_state ("malloc", p); /* { dg-warning "state: 'unchecked'" } */
  free (p);
  __analyzer_dump_state ("malloc", p); /* { dg-warning "state: 'freed'" } */
  __analyzer_dump_state (NULL, p); /* { dg-error "cannot determine state machine" } */
  __analyzer_dump_state ("not a state machine", p); /* { dg-error "unrecognized state machine 'not a state machine'" } */
}
