// TODO: remove need for this option:
/* { dg-additional-options "-fanalyzer-checker=taint" } */

#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct foo
{
  int num;
};

/* malloc with tainted size from a field.  */

void *test_1 (FILE *f)
{
  struct foo tmp;
  fread(&tmp, sizeof(tmp), 1, f); /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */

  __analyzer_dump_state ("taint", tmp.num); /* { dg-warning "state: 'tainted'" } */
  __analyzer_dump_state ("taint", tmp.num * 16); /* { dg-warning "state: 'tainted'" } */

  return malloc (tmp.num * 16); /* { dg-warning "use of attacker-controlled value 'tmp\\.num \\* 16' as allocation size without upper-bounds checking" "warning" } */
  /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'tmp\\.num \\* 16' as allocation size without upper-bounds checking" "final event with expr" { target *-*-* } .-1 } */
  // TODO: show where tmp.num * 16 gets the bogus value
}
