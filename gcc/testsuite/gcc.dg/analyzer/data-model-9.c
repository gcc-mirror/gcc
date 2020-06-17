#include <stdlib.h>
#include <string.h>
#include "analyzer-decls.h"

struct foo
{
  int i;
};

/* TODO: verify that we know that calloc zeros its memory.  */

void test_1 (void)
{
  struct foo *f = calloc (1, sizeof (struct foo));
  if (f == NULL)
    return;
  __analyzer_eval (f->i == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  free (f);
}

/* TODO: verify that we know the behavior of memset.  */

void test_2 (void)
{
  struct foo *f = malloc (sizeof (struct foo));
  if (f == NULL)
    return;
  memset (f, 0, sizeof (struct foo));
  __analyzer_eval (f->i == 0); /* { dg-warning "TRUE" "desired" { xfail *-*-* } } */
  /* { dg-bogus "UNKNOWN" "status quo" { xfail *-*-* } .-1 } */
  free (f);
}
