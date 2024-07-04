/* { dg-additional-options "-fdump-analyzer-untracked" } */

#include "analyzer-decls.h"

struct S
{
  int i;
};

/* memset of a static struct that never gets used.  */

void
test_1 (void)
{
  static struct S s; /* { dg-warning "track 's': no" } */
  __builtin_memset (&s, 0, sizeof (s));
}

/* memset of a static struct that later gets used.  */

void
test_2 (void)
{
  static struct S s; /* { dg-warning "track 's': yes" } */
  __builtin_memset (&s, 0, sizeof (s));
  __analyzer_eval (s.i == 0); /* { dg-warning "TRUE" } */
}
