/* { dg-additional-options "-Wno-incompatible-pointer-types" } */

#include <stdlib.h>

struct foo;
struct bar;
void *hv (struct foo **tm)
{
  void *p = __builtin_malloc (4);
  *tm = p;
  if (!p)
    abort ();
  return p; /* { dg-warning "leak of 'tm'" } */
}

void a5 (void)
{
  struct bar *qb = NULL;
  hv (&qb);
} /* { dg-warning "leak of '\\(struct foo \\*\\)qb'" } */
