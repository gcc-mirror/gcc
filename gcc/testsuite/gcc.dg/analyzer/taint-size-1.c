// TODO: remove need for this option:
/* { dg-additional-options "-fanalyzer-checker=taint" } */

#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct foo
{
  size_t sz;
};

char buf[100];
char buf2[100];

/* memset with tainted size.  */

void test_1 (FILE *f)
{
  struct foo tmp;
  if (1 == fread(&tmp, sizeof(tmp), 1, f)) { /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */
                                             /* { dg-message "\\(\[0-9\]+\\) following 'true' branch\\.\\.\\." "event: following true branch" { target *-*-* } .-1 } */
    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'tainted'" } */
    /* { dg-message "\\(\[0-9\]+\\) \\.\\.\\.to here" "event: to here" { target *-*-* } .-1 } */

    memset (buf, 0, tmp.sz); /* { dg-warning "use of attacker-controlled value 'tmp\\.sz' as size without upper-bounds checking" "warning" } */
    /* { dg-message "23: \\(\[0-9\]+\\) 'tmp.i' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.i has an unchecked value" { xfail *-*-* } .-1 } */
    /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'tmp\\.sz' as size without upper-bounds checking" "final event" { target *-*-* } .-2 } */
    
    // TOOD: better messages for state changes
  }
}

/* memcpy with tainted size.  */

void __attribute__((tainted_args))
test_2 (size_t sz)
{
  memcpy (buf, buf2, sz); /* { dg-warning "use of attacker-controlled value 'sz' as size without upper-bounds checking" } */
}
