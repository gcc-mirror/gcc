#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct foo
{
  signed int i;
  char buf[256];
};

char test_1(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) { /* { dg-message "\\(\[0-9\]+\\) 'tmp' gets an unchecked value here" "event: tmp gets unchecked value" { xfail *-*-* } } */
                                             /* { dg-message "\\(\[0-9\]+\\) following 'true' branch\\.\\.\\." "event: following true branch" { target *-*-* } .-1 } */
    /* BUG: the following array lookup trusts that the input data's index is
       in the range 0 <= i < 256; otherwise it's accessing the stack */
    return tmp.buf[tmp.i]; // { dg-warning "use of attacker-controlled value 'tmp.i' in array lookup without bounds checking" "warning" } */
    /* { dg-message "23: \\(\[0-9\]+\\) \\.\\.\\.to here" "event: to here" { target *-*-* } .-1 } */
    /* { dg-message "23: \\(\[0-9\]+\\) 'tmp.i' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.i has an unchecked value" { xfail *-*-* } .-2 } */
    /* { dg-message "\\(\[0-9\]+\\) use of attacker-controlled value 'tmp.i' in array lookup without bounds checking" "final event" { target *-*-* } .-3 } */
    
    // TOOD: better messages for state changes
  }
  return 0;
}

char test_2(struct foo *f, int i)
{
  /* not a bug: the data is not known to be tainted: */
  return f->buf[f->i];
}

char test_3(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.i >= 0 && tmp.i < 256) {
      /* not a bug: the access is guarded by upper and lower bounds: */
      return tmp.buf[tmp.i];
    }
  }
  return 0;
}

char test_4(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.i >= 0) { /* { dg-message "'tmp.i' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.i has an unchecked value" { xfail *-*-* } } */
      /* { dg-message "'tmp.i' has its lower bound checked here" "event: lower bound checked" { xfail *-*-* } .-1 } */
      return tmp.buf[tmp.i]; /* { dg-warning "use of attacker-controlled value 'tmp.i' in array lookup without upper-bounds checking" "warning" } */
    }
  }
  return 0;
}

char test_5(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.i < 256) { /* { dg-message "'tmp.i' has an unchecked value here \\(from 'tmp'\\)" "event: tmp.i has an unchecked value" { xfail *-*-* } } */
      /* { dg-message "'tmp.i' has its upper bound checked here" "event: upper bound checked" { xfail *-*-* } .-1 } */
      return tmp.buf[tmp.i]; /* { dg-warning "use of attacker-controlled value 'tmp.i' in array lookup without checking for negative" "warning" } */
    }
  }
  return 0;
}

/* unsigned types have a natural lower bound of 0 */
struct bar
{
  unsigned int i;
  char buf[256];
};

char test_6(FILE *f)
{
  struct bar tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    return tmp.buf[tmp.i]; /* { dg-warning "use of attacker-controlled value 'tmp.i' in array lookup without upper-bounds checking" } */
  }
  return 0;
}

char test_7(FILE *f)
{
  struct bar tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.i >= 0) {
      return tmp.buf[tmp.i]; /* { dg-warning "use of attacker-controlled value 'tmp.i' in array lookup without upper-bounds checking" } */
    }
  }
  return 0;
}

char test_8(FILE *f)
{
  struct bar tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.i < 256) {
      /* not a bug: has an upper bound, and an implicit lower bound: */
      return tmp.buf[tmp.i];
    }
  }
  return 0;
}

char test_9(FILE *f)
{
  struct foo tmp;

  if (1 == fread(&tmp, sizeof(tmp), 1, f)) {
    if (tmp.i == 42) {
      /* not a bug: tmp.i compared against a specific value: */
      return tmp.buf[tmp.i]; /* { dg-bogus "attacker-controlled" "" { xfail *-*-* } } */
      // TODO: xfail
    }
  }
  return 0;
}
