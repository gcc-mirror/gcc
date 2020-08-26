/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

void test (int *p, int a, int b, int count)
{
  int n = count;
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  while (n--)
    {
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2|3 processed enodes" } */
      *p++ = a;
      *p++ = b;
    }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
