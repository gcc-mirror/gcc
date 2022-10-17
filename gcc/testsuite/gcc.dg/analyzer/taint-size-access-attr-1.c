/* Passing tainted sizes to external functions with attribute ((access)) with
   a size-index.  */

// TODO: remove need for the explicit taint option:
/* { dg-additional-options "-fanalyzer-checker=taint -fanalyzer-show-duplicate-count" } */

#include "analyzer-decls.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct foo
{
  size_t sz;
};

char buf[100];

extern void extern_fn_read_only (void *p, size_t sz) /* { dg-message "parameter 2 of 'extern_fn_read_only' marked as a size via attribute 'access \\(read_only, 1, 2\\)'" } */
  __attribute__ ((access (read_only, 1, 2)));

void test_fn_read_only (FILE *f, void *p)
{
  struct foo tmp;
  if (1 == fread(&tmp, sizeof(tmp), 1, f)) { /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */
                                             /* { dg-message "\\(\[0-9\]+\\) following 'true' branch\\.\\.\\." "event: following true branch" { target *-*-* } .-1 } */
    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'tainted'" } */
    /* { dg-message "\\(\[0-9\]+\\) \\.\\.\\.to here" "event: to here" { target *-*-* } .-1 } */

    extern_fn_read_only (p, tmp.sz); /* { dg-warning "use of attacker-controlled value 'tmp.sz' as size without upper-bounds checking" "warning" } */
    /* { dg-bogus "duplicate" "duplicate" { target *-*-* } .-1 } */
  }
}

/* We shouldn't complain if the value has been sanitized.  */

void test_fn_sanitized (FILE *f, void *p)
{
  struct foo tmp;
  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'tainted'" } */

    if (tmp.sz > 100)
      return;

    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'has_ub'" } */
    
    extern_fn_read_only (p, tmp.sz); /* { dg-bogus "use of attacker-controlled value" } */
  }
}

/* We shouldn't complain if there was no size annotation.  */

extern void extern_fn_no_size (void *p)
  __attribute__ ((access (read_only, 1)));

void test_fn_no_size (FILE *f, void *p)
{
  struct foo tmp;
  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    __analyzer_dump_state ("taint", tmp.sz); /* { dg-warning "state: 'tainted'" } */
    extern_fn_no_size (p);
  }
}
