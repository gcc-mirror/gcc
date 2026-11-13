/* { dg-additional-options "-Wno-analyzer-too-complex" } */

#include "analyzer-decls.h"

void
test_store_heap_pointer_at_symbolic_index (void **p, int n)
{
  int i;
  for (i = 0; i < n; i++)
    p[i] = __builtin_malloc (16); /* { dg-bogus "leak" } */
}
