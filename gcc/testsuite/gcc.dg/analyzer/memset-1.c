#include <string.h>
#include "analyzer-decls.h"

/* Zero-fill of uninitialized buffer.  */

void test_1 (void)
{
  char buf[256];
  memset (buf, 0, 256);
  __analyzer_eval (buf[42] == 0); /* { dg-warning "TRUE" } */
}

/* As above, but with __builtin_memset.  */

void test_1a (void)
{
  char buf[256];
  __builtin_memset (buf, 0, 256);
  __analyzer_eval (buf[42] == 0); /* { dg-warning "TRUE" } */
}

/* Zero-fill of partially initialized buffer.  */

void test_2 (void)
{
  char buf[256];
  buf[42] = 'A';
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
  memset (buf, 0, 256);
  __analyzer_eval (buf[42] == '\0'); /* { dg-warning "TRUE" } */
}

/* A "memset" with known non-zero value.  */

void test_3 (int val)
{
  char buf[256];
  memset (buf, 'A', 256);
  /* We currently merely mark such regions as "unknown", so querying
     values within them yields UNKNOWN when ideally it would be TRUE.  */
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" "known nonzero" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
}

/* A "memset" with unknown value.  */

void test_4 (int val)
{
  char buf[256];
  memset (buf, val, 256);
  /* We currently merely mark such regions as "unknown", so querying
     values within them yields UNKNOWN when ideally it would be TRUE.  */
  __analyzer_eval (buf[42] == (char)val); /* { dg-warning "TRUE" "known nonzero" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
}

/* A "memset" with unknown num bytes.  */

void test_5 (int n)
{
  char buf[256];
  buf[42] = 'A';
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
  memset (buf, 0, n);

  /* We can't know if buf[42] was written to or not.  */
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (buf[42] == '\0'); /* { dg-warning "UNKNOWN" } */
}

/* As test_5, but with "__builtin___memset_chk".  */

void test_5a (int n)
{
  char buf[256];
  buf[42] = 'A';
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */
  __builtin___memset_chk (buf, 0, n, __builtin_object_size (buf, 0));

  /* We can't know if buf[42] was written to or not.  */
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "UNKNOWN" } */
  __analyzer_eval (buf[42] == '\0'); /* { dg-warning "UNKNOWN" } */
}

/* A "memset" with unknown value, but with zero size.  */

static size_t __attribute__((noinline))
get_zero (void)
{
  return 0;
}

void test_6 (int val)
{
  char buf[256];
  buf[42] = 'A';
  memset (buf, 'B', get_zero ());
  __analyzer_eval (buf[42] == 'A'); /* { dg-warning "TRUE" } */  
}

/* A "memset" of known size that's not the full buffer.  */

void test_7 (void)
{
  char buf[256];
  buf[128] = 'A';
  memset (buf, 0, 128);
  /* We currently merely mark the whole region as "unknown", so querying
     values within them yields UNKNOWN.  */
  __analyzer_eval (buf[127] == '\0'); /* { dg-warning "TRUE" "known nonzero" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  __analyzer_eval (buf[128] == 'A'); /* { dg-warning "TRUE" "known nonzero" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
}
