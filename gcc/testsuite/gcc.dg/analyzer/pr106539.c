#include <stdlib.h>

void *test (void)
{
  void **p = (void **)malloc (sizeof (void *) * 2);
  if (!p)
    return NULL;
  p[0] = malloc(10);
  p[1] = malloc(20); /* { dg-message "allocated here" }  */
  void *q = realloc (p, sizeof (void *)); /* { dg-message "when 'realloc' succeeds, moving buffer" } */
  if (!q)
  /* { dg-warning "leak of '<unknown>'" "leak of unknown" { target *-*-* } .-1 } */
    return p;
  return q;
}
