/* { dg-additional-options "-Wno-incompatible-pointer-types" } */
/* C only: Wno-incompatible-pointer-types is not valid for C++. */

#include <stdlib.h>

struct foo;
struct bar;
void *hv (struct foo **tm)
{
  void *p = __builtin_malloc (4);
  *tm = (struct foo *) p;
  if (!p)
    abort ();
  return p;
}

void a5 (void)
{
  struct bar *qb = NULL;
  hv ((struct foo **) &qb);
} /* { dg-warning "leak of 'qb'" } */
