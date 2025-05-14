#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct foo
{
  size_t sz;
};

/* malloc with tainted size.  */

void *test_1 (FILE *f)
{
  struct foo tmp;
  if (1 == fread(&tmp, sizeof(tmp), 1, f)) { /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */
                                             /* { dg-message "\\(\[0-9\]+\\) following 'true' branch\\.\\.\\." "event: following true branch" { target *-*-* } .-1 } */
    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'tainted'" } */
    /* { dg-message "\\(\[0-9\]+\\) \\.\\.\\.to here" "event: to here" { target *-*-* } .-1 } */
    
    return malloc (tmp.sz); /* { dg-warning "use of attacker-controlled value 'tmp\\.sz' as allocation size without upper-bounds checking" "warning" } */
    /* { dg-message "23: \\(\[0-9\]+\\) 'tmp.i' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.i has an unchecked value" { xfail *-*-* } .-1 } */
    /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'tmp\\.sz' as allocation size without upper-bounds checking" "final event" { target *-*-* } .-2 } */
    /* { dg-message "heap-based allocation" "memory space" { target *-*-* } .-3 } */
    
    // TOOD: better messages for state changes
  }
  return 0;
}

/* VLA with tainted size.  */

void *test_2 (FILE *f)
{
  struct foo tmp;
  if (1 == fread(&tmp, sizeof(tmp), 1, f)) { /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */
                                             /* { dg-message "\\(\[0-9\]+\\) following 'true' branch\\.\\.\\." "event: following true branch" { target *-*-* } .-1 } */
    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'tainted'" } */
    /* { dg-message "\\(\[0-9\]+\\) \\.\\.\\.to here" "event: to here" { target *-*-* } .-1 } */

    /* VLA with tainted size.  */
    {
      char buf[tmp.sz]; /* { dg-warning "use of attacker-controlled value 'tmp\\.sz' as allocation size without upper-bounds checking" "warning" } */
      /* { dg-message "\\(\[0-9\]+\\) 'tmp.i' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.i has an unchecked value" { xfail *-*-* } .-1 } */
      /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'tmp\\.sz' as allocation size without upper-bounds checking" "final event" { target *-*-* } .-2 } */
      /* { dg-message "stack-based allocation" "memory space" { target *-*-* } .-3 } */
      fread (buf, tmp.sz, 1, f);
    }
    
    // TOOD: better messages for state changes
  }
  return 0;
}

void *test_3 (FILE *f)
{
  int num;
  fread (&num, sizeof (int), 1, f);
  __analyzer_dump_state ("taint", num); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", num * 16); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", (size_t)(num * 16)); /* { dg-warning "state: 'tainted'" } */
  return malloc (num * 16); /* { dg-warning "use of attacker-controlled value 'num \\* 16' as allocation size without upper-bounds checking" } */
}
