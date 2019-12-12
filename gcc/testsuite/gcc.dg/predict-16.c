/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */

#include <stdlib.h>
#include <string.h>

void *r;
void *r2;
void *r3;
void *r4;
void *r5;

void *m (size_t s, int c)
{
  r = malloc (s);
  if (r)
    memset (r, 0, s);

  r2 = calloc (s, 0);
  if (r2)
    memset (r2, 0, s);

  r3 = __builtin_malloc (s);
  if (r3)
    memset (r3, 0, s);

  r4 = __builtin_calloc (s, 0);
  if (r4)
    memset (r4, 0, s);

  r5 = __builtin_realloc (r4, s);
  if (r5)
    memset (r4, 0, s);
}

/* { dg-final { scan-tree-dump-times "malloc returned non-NULL heuristics of edge\[^:\]*: 99.96%" 5 "profile_estimate"} } */
