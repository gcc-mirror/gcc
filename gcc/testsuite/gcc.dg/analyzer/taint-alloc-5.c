#include "analyzer-decls.h"

struct foo
{
  int num;
};

/* malloc with tainted size from a field.  */

void * __attribute__ ((tainted_args))
test_1 (struct foo f)
{
  __analyzer_dump_state ("taint", f.num); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", f.num * 16); /* { dg-warning "state: 'tainted'" } */

  return __builtin_malloc (f.num * 16); /* { dg-warning "use of attacker-controlled value 'f\\.num \\* 16' as allocation size without upper-bounds checking" "warning" } */
  /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'f\\.num \\* 16' as allocation size without upper-bounds checking" "final event with expr" { target *-*-* } .-1 } */
}
