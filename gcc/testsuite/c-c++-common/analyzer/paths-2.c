#include "analyzer-decls.h"

int test (int a)
{
  if (a != 42 && a != 113) {
    return (-2);
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  return 0;
}

int test_2 (int a)
{
  if (a != 42 && a != 113 && a != 666) {
    return (-2);
  }

  __analyzer_dump_exploded_nodes (0); /* { dg-warning "1 processed enode" } */

  return 0;
}
