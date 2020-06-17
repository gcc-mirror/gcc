#include "analyzer-decls.h"

void test (int *p, int n)
{
  int i;
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
  for (i = 0; i < n; i++)
    {
      p[i] = i; /* { dg-bogus "uninitialized" } */
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
    }
}
