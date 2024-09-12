/* { dg-skip-if "requires hosted libstdc++ for stdlib realloc" { ! hostedlib } } */

#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* realloc with tainted size.  */

void *p;

void __attribute__((tainted_args))
test_1 (size_t sz) /* { dg-message "\\(1\\) function 'test_1' marked with '__attribute__\\(\\(tainted_args\\)\\)'" } */
{
  void *q;
  
  __analyzer_dump_state ("taint", sz); /* { dg-warning "state: 'tainted'" } */

  q = realloc (p, sz);  /* { dg-warning "use of attacker-controlled value 'sz' as allocation size without upper-bounds checking" } */
} /* { dg-warning "leak of 'q'" } */
