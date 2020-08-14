/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

#include "../analyzer-decls.h"

void test (int *p)
{
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  while (*p)
    {
      __analyzer_dump_exploded_nodes (0); /* { dg-warning "2 processed enode" } */
      p++;
    }
  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */
}
