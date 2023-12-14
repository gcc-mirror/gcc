/* { dg-additional-options "-fdump-analyzer-untracked" } */

#include "analyzer-decls.h"

struct S
{
  char buf[10];
};

/* strcpy to a static struct that never gets used.  */

void
test_1 (const char *src)
{
  static struct S s; /* { dg-warning "track 's': no" } */
  __builtin_strcpy (s.buf, src);
}

/* strcpy to a static struct that later gets used.  */

const char *
test_2 (const char *src)
{
  static struct S s; /* { dg-warning "track 's': yes" } */
  __builtin_strcpy (s.buf, src);
  return s.buf;
}
