/* { dg-additional-options "-fdump-analyzer-untracked" } */

#include "../../gcc.dg/analyzer/analyzer-decls.h"

struct S
{
  int i;
};

typedef __SIZE_TYPE__ size_t;

extern size_t fread (void *, size_t, size_t, void *);

/* fread of a static struct that never gets used.  */

void
test_1 (void *fp)
{
  static struct S s; /* { dg-warning "track 's': no" } */
  fread (&s, sizeof (s), 1, fp);
}

/* fread of a static struct that later gets used.  */

int
test_2 (void *fp)
{
  static struct S s; /* { dg-warning "track 's': yes" } */
  fread (&s, sizeof (s), 1, fp);
  return s.i;
}
