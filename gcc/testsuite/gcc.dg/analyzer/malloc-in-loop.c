#include <stdlib.h>
#include "analyzer-decls.h"

extern void foo (int *);

void test (int n)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  for (int i = 0; i < n; i++)
    {
      int *ptr = (int *)malloc (sizeof (int) * i);
      foo (ptr);
      free (ptr);
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enodes" } */
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
