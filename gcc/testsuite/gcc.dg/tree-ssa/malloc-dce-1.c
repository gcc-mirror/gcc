/* { dg-do compile } */
/* { dg-options "-O2 -fmalloc-dce=1 -fdump-tree-optimized" } */
#include <stdlib.h>

void
pair_only (size_t n)
{
  free (malloc (n)); /* removed */
}

int
ne (size_t n)
{
  void *p = malloc (n);
  int r = p != NULL;
  free (p);
  return r; /* malloc must stay */
}

int
eq (size_t n)
{
  void *p = malloc (n);
  int r = p == NULL;
  free (p);
  return r; /* malloc must stay */
}

/* { dg-final { scan-tree-dump-times "malloc \\(" 2 "optimized" } } */
