/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

void test (int *p, int val, int count)
{
  int n = count;
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  while (n--)
    {
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "3 processed enodes" } */
      *p++ = val;
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
