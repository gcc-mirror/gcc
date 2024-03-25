#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* malloc with tainted size from a syscall.  */

void *p;

void __attribute__((tainted_args))
test_1 (size_t sz) /* { dg-message "\\(1\\) function 'test_1' marked with '__attribute__\\(\\(tainted_args\\)\\)'" } */
{
  /* TODO: should have a message saying why "sz" is tainted, e.g.
     "treating 'sz' as attacker-controlled because 'test_1' is marked with '__attribute__((tainted_args))'"  */

  p = malloc (sz); /* { dg-warning "use of attacker-controlled value 'sz' as allocation size without upper-bounds checking" "warning" } */
  /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'sz' as allocation size without upper-bounds checking" "final event" { target *-*-* } .-1 } */
}
