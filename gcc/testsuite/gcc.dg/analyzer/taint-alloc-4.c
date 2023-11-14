#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* malloc with tainted size from a syscall.  */

struct arg_buf
{
  size_t sz;
};

void *p;

void __attribute__((tainted_args))
test_1 (void *data) /* { dg-message "\\(1\\) function 'test_1' marked with '__attribute__\\(\\(tainted_args\\)\\)'" } */
{
  /* we should treat pointed-to-structs as tainted.  */
  __analyzer_dump_state ("taint", data); /* { dg-warning "state: 'tainted'" } */
  
  struct arg_buf *args = data;

  __analyzer_dump_state ("taint", args); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", args->sz); /* { dg-warning "state: 'tainted'" } */
  
  p = malloc (args->sz); /* { dg-warning "use of attacker-controlled value '\\*args.sz' as allocation size without upper-bounds checking" "warning" } */
  /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value '\\*args.sz' as allocation size without upper-bounds checking" "final event" { target *-*-* } .-1 } */
}
