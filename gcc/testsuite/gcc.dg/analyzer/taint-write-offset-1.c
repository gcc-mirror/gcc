// TODO: remove need for this option:
/* { dg-additional-options "-fanalyzer-checker=taint" } */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct foo
{
  ssize_t offset;
};

char *p;

char test_1(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) { /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */
                                             /* { dg-message "\\(\[0-9\]+\\) following 'true' branch\\.\\.\\." "event: following true branch" { target *-*-* } .-1 } */
    /* BUG: the following array lookup trusts that the input data's index is
       in the range 0 <= i < 256; otherwise it's accessing the stack */
    *(p + tmp.offset) = 42; // { dg-warning "use of attacker-controlled value 'tmp.offset' as offset without bounds checking" "warning" } */
    /* { dg-message "\\(\[0-9\]+\\) \\.\\.\\.to here" "event: to here" { target *-*-* } .-1 } */
    /* { dg-message "\\(\[0-9\]+\\) 'tmp.offset' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.offset has an unchecked value" { xfail *-*-* } .-2 } */
    /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'tmp.offset' as offset without bounds checking" "final event" { target *-*-* } .-3 } */
    
    // TOOD: better messages for state changes
  }
  return 0;
}

char test_2(struct foo *f)
{
  /* not a bug: the data is not known to be tainted: */
  *(p + f->offset) = 42;
}

char test_3(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.offset >= 0 && tmp.offset < 256) {
      /* not a bug: the access is guarded by upper and lower bounds: */
      *(p + tmp.offset) = 42;
    }
  }
  return 0;
}

char test_4(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.offset >= 0) { /* { dg-message "'tmp.offset' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.offset has an unchecked value" { xfail *-*-* } } */
      /* { dg-message "'tmp.offset' has its lower bound checked here" "event: lower bound checked" { xfail *-*-* } .-1 } */
      *(p + tmp.offset) = 42; /* { dg-warning "use of attacker-controlled value 'tmp.offset' as offset without upper-bounds checking" "warning" } */
    }
  }
  return 0;
}

char test_5(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.offset < 256) { /* { dg-message "'tmp.offset' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.offset has an unchecked value" { xfail *-*-* } } */
      /* { dg-message "'tmp.offset' has its upper bound checked here" "event: upper bound checked" { xfail *-*-* } .-1 } */
      *(p + tmp.offset) = 42; /* { dg-warning "use of attacker-controlled value 'tmp.offset' as offset without lower-bounds checking" "warning" } */
    }
  }
  return 0;
}

/* unsigned types have a natural lower bound of 0 */
struct bar
{
  unsigned int offset;
  char buf[256];
};

char test_6(FILE *f)
{
  struct bar tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    *(p + tmp.offset) = 42; /* { dg-warning "use of attacker-controlled value 'tmp.offset' as offset without upper-bounds checking" } */
  }
  return 0;
}

char test_7(FILE *f)
{
  struct bar tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.offset >= 0) {
      *(p + tmp.offset) = 42; /* { dg-warning "use of attacker-controlled value 'tmp.offset' as offset without upper-bounds checking" } */
    }
  }
  return 0;
}

char test_8(FILE *f)
{
  struct bar tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.offset < 256) {
      /* not a bug: has an upper bound, and an implicit lower bound: */
      *(p + tmp.offset) = 42;
    }
  }
  return 0;
}

char test_9(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.offset == 42) {
      /* not a bug: tmp.offset compared against a specific value: */
      *(p + tmp.offset) = 42; /* { dg-bogus "tainted" "" { xfail *-*-* } } */
      // TODO: xfail
    }
  }
  return 0;
}
